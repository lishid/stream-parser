import { Decoration, EditorView, ViewPlugin, ViewUpdate, DecorationSet } from '@codemirror/view'
import {Tree, TreeFragment, NodeType, NodeSet, SyntaxNode, PartialParse, NodeProp} from "lezer-tree"
import {Input} from "lezer"
import {Language, defineLanguageFacet, languageDataProp, IndentContext, indentService,
        EditorParseContext, getIndentUnit, syntaxTree} from "@codemirror/language"
import {EditorState, Facet, Prec} from "@codemirror/state"
import {RangeSetBuilder} from "@codemirror/rangeset"
import {StringStream} from "./stringstream"

export {StringStream}

/// A stream parser parses or tokenizes content from start to end,
/// emitting tokens as it goes over it. It keeps a mutable (but
/// copyable) object with state, in which it can store information
/// about the current context.
export interface StreamParser<State> {
  /// Read one token, advancing the stream past it, and returning a
  /// string indicating the token's style tag—either the name of one
  /// of the tags in [`tags`](#highlight.tags), or such a name
  /// suffixed by one or more tag
  /// [modifier](#highlight.Tag^defineModifier) names, separated by
  /// spaces. For example `"keyword"` or "`variableName.constant"`.
  ///
  /// It is okay to return a zero-length token, but only if that
  /// updates the state so that the next call will return a non-empty
  /// token again.
  token(stream: StringStream, state: State): string | null
  /// This notifies the parser of a blank line in the input. It can
  /// update its state here if it needs to.
  blankLine?(state: State, indentUnit: number): void
  /// Produce a start state for the parser.
  startState?(indentUnit: number): State
  /// Copy a given state. By default, a shallow object copy is done
  /// which also copies arrays held at the top level of the object.
  copyState?(state: State): State
  /// Compute automatic indentation for the line that starts with the
  /// given state and text.
  indent?(state: State, textAfter: string, context: IndentContext): number | null
  /// Default [language data](#state.EditorState.languageDataAt) to
  /// attach to this language.
  languageData?: {[name: string]: any}
}

function fullParser<State>(spec: StreamParser<State>): Required<StreamParser<State>> {
  return {
    token: spec.token,
    blankLine: spec.blankLine || (() => {}),
    startState: spec.startState || (() => (true as any)),
    copyState: spec.copyState || defaultCopyState,
    indent: spec.indent || (() => null),
    languageData: spec.languageData || {}
  }
}

function defaultCopyState<State>(state: State) {
  if (typeof state != "object") return state
  let newState = {} as State
  for (let prop in state) {
    let val = state[prop]
    newState[prop] = (val instanceof Array ? val.slice() : val) as any
  }
  return newState
}

/// A [language](#language.Language) class based on a streaming
/// parser.
export class StreamLanguage<State> extends Language {
  /// @internal
  streamParser: Required<StreamParser<State>>
  /// @internal
  stateAfter: WeakMap<Tree, State>

  private constructor(parser: StreamParser<State>) {
    let data = defineLanguageFacet(parser.languageData)
    let p = fullParser(parser)
    let startParse = (input: Input, startPos: number, context: EditorParseContext) => new Parse(this, input, startPos, context)
    super(data, {startParse}, docID(data), [indentService.of((cx, pos) => this.getIndent(cx, pos))])
    this.streamParser = p
    this.stateAfter = new WeakMap
  }

  static define<State>(spec: StreamParser<State>) { return new StreamLanguage(spec) }

  private getIndent(cx: IndentContext, pos: number) {
    let tree = syntaxTree(cx.state), at: SyntaxNode | null = tree.resolve(pos)
    while (at && at.type != this.topNode) at = at.parent
    if (!at) return null
    let start = findState(this, tree, 0, at.from, pos), statePos: number, state
    if (start) { state = start.state; statePos = start.pos + 1 }
    else { state = this.streamParser.startState(cx.unit) ; statePos = 0 }
    if (pos - statePos > C.MaxIndentScanDist) return null
    while (statePos < pos) {
      let line = cx.state.doc.lineAt(statePos), end = Math.min(pos, line.to)
      if (line.length) {
        let lookahead = (n: number) => cx.state.doc.line(n + line.number).text
        let stream = new StringStream(line.text, cx.state.tabSize, cx.unit, lookahead)
        while (stream.pos < end - line.from)
          readToken(this.streamParser.token, stream, state)
      } else {
        this.streamParser.blankLine(state, cx.unit)
      }
      if (end == pos) break
      statePos = line.to + 1
    }
    let {text} = cx.state.doc.lineAt(pos)
    return this.streamParser.indent(state, /^\s*(.*)/.exec(text)![1], cx)
  }

  get allowsNesting() { return false }
}

function findState<State>(
  lang: StreamLanguage<State>, tree: Tree, off: number, startPos: number, before: number
): {state: State, pos: number} | null {
  let state = off >= startPos && off + tree.length <= before && lang.stateAfter.get(tree)
  if (state) return {state: lang.streamParser.copyState(state), pos: off + tree.length}
  for (let i = tree.children.length - 1; i >= 0; i--) {
    let child = tree.children[i], pos = off + tree.positions[i]
    let found = child instanceof Tree && pos < before && findState(lang, child, pos, startPos, before)
    if (found) return found
  }
  return null
}

function cutTree(lang: StreamLanguage<unknown>, tree: Tree, from: number, to: number, inside: boolean): Tree | null {
  if (inside && from <= 0 && to >= tree.length) return tree
  if (!inside && tree.type == lang.topNode) inside = true
  for (let i = tree.children.length - 1; i >= 0; i--) {
    let pos = tree.positions[i] + from, child = tree.children[i], inner
    if (pos < to && child instanceof Tree) {
      if (!(inner = cutTree(lang, child, from - pos, to - pos, inside))) break
      return !inside ? inner
        : new Tree(tree.type, tree.children.slice(0, i).concat(inner), tree.positions.slice(0, i + 1), pos + inner.length)
    }
  }
  return null
}

function findStartInFragments<State>(lang: StreamLanguage<State>, fragments: readonly TreeFragment[],
                                     startPos: number, state: EditorState) {
  for (let f of fragments) {
    let found = f.from <= startPos && f.to > startPos && findState(lang, f.tree, 0 - f.offset, startPos, f.to), tree
    if (found && (tree = cutTree(lang, f.tree, startPos + f.offset, found.pos + f.offset, false)))
      return {state: found.state, tree}
  }
  return {state: lang.streamParser.startState(getIndentUnit(state)), tree: Tree.empty}
}

const enum C {
  ChunkSize = 2048,
  MaxDistanceBeforeViewport = 1e5,
  MaxIndentScanDist = 1e4
}

class Parse<State> implements PartialParse {
  state: State
  pos: number
  chunks: Tree[] = []
  chunkPos: number[] = []
  chunkStart: number
  chunk: number[] = []

  constructor(readonly lang: StreamLanguage<State>,
              readonly input: Input,
              readonly startPos: number,
              readonly context: EditorParseContext) {
    let {state, tree} = findStartInFragments(lang, context.fragments, startPos, context.state)
    this.state = state
    this.pos = this.chunkStart = startPos + tree.length
    if (tree.length) {
      this.chunks.push(tree)
      this.chunkPos.push(0)
    }
    if (this.pos < context.viewport.from - C.MaxDistanceBeforeViewport) {
      this.state = this.lang.streamParser.startState(getIndentUnit(context.state))
      context.skipUntilInView(this.pos, context.viewport.from)
      this.pos = context.viewport.from
    }
  }

  advance() {
    let end = Math.min(this.context.viewport.to, this.input.length, this.chunkStart + C.ChunkSize)
    while (this.pos < end) this.parseLine()
    if (this.chunkStart < this.pos) this.finishChunk()
    if (end < this.input.length && this.pos < this.context.viewport.to) return null
    this.context.skipUntilInView(this.pos, this.input.length)
    return this.finish()
  }

  parseLine() {
    let line = this.input.lineAfter(this.pos), {streamParser} = this.lang
    let lookahead = (n: number) => {
      let pos = this.pos
      for (let i = 0; i < n; i++) {
        pos += this.input.lineAfter(pos).length
        if (pos < this.input.length) pos++
      }
      return this.input.lineAfter(pos)
    }
    let stream = new StringStream(line, this.context ? this.context.state.tabSize : 4, getIndentUnit(this.context.state), lookahead)
    if (stream.eol()) {
      streamParser.blankLine(this.state, stream.indentUnit)
    } else {
      let lineClasses: string[] = []
      let chunks = [];
      let last = null;
      while (!stream.eol()) {
        let token = readToken(streamParser.token, stream, this.state)
        if (token) {
          let classes = token.split(' ').filter(t => {
            if (!t) return false
            let lineClass = t.match(/(?:^|\s+)line-(background-)?(\S+)/)
            if (lineClass) {
              lineClasses.push(lineClass[2])
              return false
            }
            return true
          })
          let cls = classes.sort().join(' ')
          let from = this.pos + stream.start
          let to = this.pos + stream.pos
          if (last && last.cls === cls && last.to === from) {
            last.to = to
          } else {
            last = {cls, from, to}
            chunks.push(last)
          }
        }
      }
      for (let chunk of chunks) {
        this.chunk.push(tokenID(chunk.cls), chunk.from, chunk.to, 4)
      }
      if (lineClasses.length > 0) {
        this.chunk.push(tokenID(lineClasses.join(' '), true), this.pos, this.pos + line.length, (chunks.length + 1) * 4)
      }
    }
    this.pos += line.length
    if (this.pos < this.input.length) this.pos++
  }

  finishChunk() {
    let tree = Tree.build({
      buffer: this.chunk,
      start: this.chunkStart,
      length: this.pos - this.chunkStart,
      nodeSet,
      topID: 0,
      maxBufferLength: C.ChunkSize
    })
    this.lang.stateAfter.set(tree, this.lang.streamParser.copyState(this.state))
    this.chunks.push(tree)
    this.chunkPos.push(this.chunkStart - this.startPos)
    this.chunk = []
    this.chunkStart = this.pos
  }

  finish() {
    return new Tree(this.lang.topNode, this.chunks, this.chunkPos, this.pos - this.startPos).balance()
  }

  forceFinish() {
    return this.finish()
  }
}

function readToken<State>(token: (stream: StringStream, state: State) => string | null,
                          stream: StringStream, state: State) {
  stream.start = stream.pos
  for (let i = 0; i < 10; i++) {
    let result = token(stream, state)
    if (stream.pos > stream.start) return result
  }
  throw new Error("Stream parser failed to advance stream.")
}

const tokenTable: {[name: string]: number} = Object.create(null)
const typeArray: NodeType[] = [NodeType.none]
const nodeSet = new NodeSet(typeArray)

function tokenID(tag: string, lineMode?: boolean): number {
  return !tag ? 0 : tokenTable[tag] || (tokenTable[tag] = createTokenType(tag, lineMode))
}

function createTokenType(tagStr: string, lineMode?: boolean) {
  let name = tagStr.replace(/ /g, "_"), type = NodeType.define({
    id: typeArray.length,
    name,
    props: [lineMode ? lineClassNodeProp.add({[name]: tagStr}) : tokenClassNodeProp.add({[name]: tagStr})]
  })
  typeArray.push(type)
  return type.id
}

function docID(data: Facet<{[name: string]: any}>) {
  let type = NodeType.define({id: typeArray.length, name: "Document", props: [languageDataProp.add(() => data)]})
  typeArray.push(type)
  return type
}

// The NodeProp that holds the css class we want to apply
export const tokenClassNodeProp = new NodeProp<string>()
export const lineClassNodeProp = new NodeProp<string>()

class LineHighlighter {
  decorations: DecorationSet
  tree: Tree
  lineCache: {[cls: string]: Decoration} = Object.create(null)
  tokenCache: {[cls: string]: Decoration} = Object.create(null)

  constructor(view: EditorView) {
    this.tree = syntaxTree(view.state)
    this.decorations = this.buildDeco(view)
  }

  update(update: ViewUpdate) {
    let tree = syntaxTree(update.state)
    if (tree.length < update.view.viewport.to) {
      this.decorations = this.decorations.map(update.changes)
    } else if (tree != this.tree || update.viewportChanged) {
      this.tree = tree
      this.decorations = this.buildDeco(update.view)
    }
  }

  buildDeco(view: EditorView) {
    if (!this.tree.length) return Decoration.none

    let builder = new RangeSetBuilder<Decoration>()
    let lastPos = 0;
    for (let {from, to} of view.visibleRanges) {
      this.tree.iterate({
        from, to,
        enter: (type, start, end) => {
          let lineStyle = type.prop(lineClassNodeProp)
          if (lineStyle) {
            let lineDeco = this.lineCache[lineStyle] || (this.lineCache[lineStyle] = Decoration.line({attributes:{class: lineStyle}}))
            let newFrom = view.visualLineAt(start).from
            // Ignore line modes that start from the middle of the line text
            if (lastPos > newFrom) {
              return;
            }
            builder.add(newFrom, newFrom, lineDeco)
            lastPos = newFrom;
          }

          let tokenStyle = type.prop(tokenClassNodeProp)
          if (tokenStyle) {
            let lineDeco = this.tokenCache[tokenStyle] || (this.tokenCache[tokenStyle] = Decoration.mark({class: tokenStyle.split(' ').map(c => 'cm-' + c).join(' ')}))
            builder.add(start, end, lineDeco)
            lastPos = end;
          }
        }
      })
    }
    return builder.finish()
  }
}

// This extension installs a highlighter that highlights lines based on the node prop above
export const lineHighlighter = Prec.fallback(ViewPlugin.define(view => new LineHighlighter(view), {
  decorations: v => v.decorations
}))
