import { ViewPlugin, Decoration } from '@codemirror/view';
import { Tree, NodeType, NodeSet, NodeProp } from 'lezer-tree';
import { Language, defineLanguageFacet, indentService, syntaxTree, languageDataProp, getIndentUnit } from '@codemirror/language';
import { Prec } from '@codemirror/state';
import { RangeSetBuilder } from '@codemirror/rangeset';
import { countColumn } from '@codemirror/text';

// Counts the column offset in a string, taking tabs into account.
// Used mostly to find indentation.
function countCol(string, end, tabSize, startIndex = 0, startValue = 0) {
    if (end == null) {
        end = string.search(/[^\s\u00a0]/);
        if (end == -1)
            end = string.length;
    }
    return countColumn(string.slice(startIndex, end), startValue, tabSize);
}
/// Encapsulates a single line of input. Given to stream syntax code,
/// which uses it to tokenize the content.
class StringStream {
    /// @internal
    constructor(
    /// The line.
    string, tabSize, 
    /// The current indent unit size.
    indentUnit, lookAhead) {
        this.string = string;
        this.tabSize = tabSize;
        this.indentUnit = indentUnit;
        this.lookAhead = lookAhead;
        /// The current position on the line.
        this.pos = 0;
        /// The start position of the current token.
        this.start = 0;
        this.lastColumnPos = 0;
        this.lastColumnValue = 0;
    }
    /// True if we are at the end of the line.
    eol() { return this.pos >= this.string.length; }
    /// True if we are at the start of the line.
    sol() { return this.pos == 0; }
    /// Get the next code unit after the current position, or undefined
    /// if we're at the end of the line.
    peek() { return this.string.charAt(this.pos) || undefined; }
    /// Read the next code unit and advance `this.pos`.
    next() {
        if (this.pos < this.string.length)
            return this.string.charAt(this.pos++);
    }
    /// Match the next character against the given string, regular
    /// expression, or predicate. Consume and return it if it matches.
    eat(match) {
        let ch = this.string.charAt(this.pos);
        let ok;
        if (typeof match == "string")
            ok = ch == match;
        else
            ok = ch && (match instanceof RegExp ? match.test(ch) : match(ch));
        if (ok) {
            ++this.pos;
            return ch;
        }
    }
    /// Continue matching characters that match the given string,
    /// regular expression, or predicate function. Return true if any
    /// characters were consumed.
    eatWhile(match) {
        let start = this.pos;
        while (this.eat(match)) { }
        return this.pos > start;
    }
    /// Consume whitespace ahead of `this.pos`. Return true if any was
    /// found.
    eatSpace() {
        let start = this.pos;
        while (/[\s\u00a0]/.test(this.string.charAt(this.pos)))
            ++this.pos;
        return this.pos > start;
    }
    /// Move to the end of the line.
    skipToEnd() { this.pos = this.string.length; }
    /// Move to directly before the given character, if found on the
    /// current line.
    skipTo(ch) {
        let found = this.string.indexOf(ch, this.pos);
        if (found > -1) {
            this.pos = found;
            return true;
        }
    }
    /// Move back `n` characters.
    backUp(n) { this.pos -= n; }
    /// Get the column position at `this.pos`.
    column() {
        if (this.lastColumnPos < this.start) {
            this.lastColumnValue = countCol(this.string, this.start, this.tabSize, this.lastColumnPos, this.lastColumnValue);
            this.lastColumnPos = this.start;
        }
        return this.lastColumnValue;
    }
    /// Get the indentation column of the current line.
    indentation() {
        return countCol(this.string, null, this.tabSize);
    }
    /// Match the input against the given string or regular expression
    /// (which should start with a `^`). Return true or the regexp match
    /// if it matches.
    ///
    /// Unless `consume` is set to `false`, this will move `this.pos`
    /// past the matched text.
    ///
    /// When matching a string `caseInsensitive` can be set to true to
    /// make the match case-insensitive.
    match(pattern, consume, caseInsensitive) {
        if (typeof pattern == "string") {
            let cased = (str) => caseInsensitive ? str.toLowerCase() : str;
            let substr = this.string.substr(this.pos, pattern.length);
            if (cased(substr) == cased(pattern)) {
                if (consume !== false)
                    this.pos += pattern.length;
                return true;
            }
            else
                return null;
        }
        else {
            let match = this.string.slice(this.pos).match(pattern);
            if (match && match.index > 0)
                return null;
            if (match && consume !== false)
                this.pos += match[0].length;
            return match;
        }
    }
    /// Get the current token.
    current() { return this.string.slice(this.start, this.pos); }
}

function fullParser(spec) {
    return {
        token: spec.token,
        blankLine: spec.blankLine || (() => { }),
        startState: spec.startState || (() => true),
        copyState: spec.copyState || defaultCopyState,
        indent: spec.indent || (() => null),
        languageData: spec.languageData || {}
    };
}
function defaultCopyState(state) {
    if (typeof state != "object")
        return state;
    let newState = {};
    for (let prop in state) {
        let val = state[prop];
        newState[prop] = (val instanceof Array ? val.slice() : val);
    }
    return newState;
}
/// A [language](#language.Language) class based on a streaming
/// parser.
class StreamLanguage extends Language {
    constructor(parser) {
        let data = defineLanguageFacet(parser.languageData);
        let p = fullParser(parser);
        let startParse = (input, startPos, context) => new Parse(this, input, startPos, context);
        super(data, { startParse }, docID(data), [indentService.of((cx, pos) => this.getIndent(cx, pos))]);
        this.streamParser = p;
        this.stateAfter = new WeakMap;
    }
    static define(spec) { return new StreamLanguage(spec); }
    getIndent(cx, pos) {
        let tree = syntaxTree(cx.state), at = tree.resolve(pos);
        while (at && at.type != this.topNode)
            at = at.parent;
        if (!at)
            return null;
        let start = findState(this, tree, 0, at.from, pos), statePos, state;
        if (start) {
            state = start.state;
            statePos = start.pos + 1;
        }
        else {
            state = this.streamParser.startState(cx.unit);
            statePos = 0;
        }
        if (pos - statePos > 10000 /* MaxIndentScanDist */)
            return null;
        while (statePos < pos) {
            let line = cx.state.doc.lineAt(statePos), end = Math.min(pos, line.to);
            if (line.length) {
                let lookahead = (n) => cx.state.doc.line(n + line.number).text;
                let stream = new StringStream(line.text, cx.state.tabSize, cx.unit, lookahead);
                while (stream.pos < end - line.from)
                    readToken(this.streamParser.token, stream, state);
            }
            else {
                this.streamParser.blankLine(state, cx.unit);
            }
            if (end == pos)
                break;
            statePos = line.to + 1;
        }
        let { text } = cx.state.doc.lineAt(pos);
        return this.streamParser.indent(state, /^\s*(.*)/.exec(text)[1], cx);
    }
    get allowsNesting() { return false; }
}
function findState(lang, tree, off, startPos, before) {
    let state = off >= startPos && off + tree.length <= before && lang.stateAfter.get(tree);
    if (state)
        return { state: lang.streamParser.copyState(state), pos: off + tree.length };
    for (let i = tree.children.length - 1; i >= 0; i--) {
        let child = tree.children[i], pos = off + tree.positions[i];
        let found = child instanceof Tree && pos < before && findState(lang, child, pos, startPos, before);
        if (found)
            return found;
    }
    return null;
}
function cutTree(lang, tree, from, to, inside) {
    if (inside && from <= 0 && to >= tree.length)
        return tree;
    if (!inside && tree.type == lang.topNode)
        inside = true;
    for (let i = tree.children.length - 1; i >= 0; i--) {
        let pos = tree.positions[i] + from, child = tree.children[i], inner;
        if (pos < to && child instanceof Tree) {
            if (!(inner = cutTree(lang, child, from - pos, to - pos, inside)))
                break;
            return !inside ? inner
                : new Tree(tree.type, tree.children.slice(0, i).concat(inner), tree.positions.slice(0, i + 1), pos + inner.length);
        }
    }
    return null;
}
function findStartInFragments(lang, fragments, startPos, state) {
    for (let f of fragments) {
        let found = f.from <= startPos && f.to > startPos && findState(lang, f.tree, 0 - f.offset, startPos, f.to), tree;
        if (found && (tree = cutTree(lang, f.tree, startPos + f.offset, found.pos + f.offset, false)))
            return { state: found.state, tree };
    }
    return { state: lang.streamParser.startState(getIndentUnit(state)), tree: Tree.empty };
}
class Parse {
    constructor(lang, input, startPos, context) {
        this.lang = lang;
        this.input = input;
        this.startPos = startPos;
        this.context = context;
        this.chunks = [];
        this.chunkPos = [];
        this.chunk = [];
        let { state, tree } = findStartInFragments(lang, context.fragments, startPos, context.state);
        this.state = state;
        this.pos = this.chunkStart = startPos + tree.length;
        if (tree.length) {
            this.chunks.push(tree);
            this.chunkPos.push(0);
        }
        if (this.pos < context.viewport.from - 100000 /* MaxDistanceBeforeViewport */) {
            this.state = this.lang.streamParser.startState(getIndentUnit(context.state));
            context.skipUntilInView(this.pos, context.viewport.from);
            this.pos = context.viewport.from;
        }
    }
    advance() {
        let end = Math.min(this.context.viewport.to, this.input.length, this.chunkStart + 2048 /* ChunkSize */);
        while (this.pos < end)
            this.parseLine();
        if (this.chunkStart < this.pos)
            this.finishChunk();
        if (end < this.input.length && this.pos < this.context.viewport.to)
            return null;
        this.context.skipUntilInView(this.pos, this.input.length);
        return this.finish();
    }
    parseLine() {
        let line = this.input.lineAfter(this.pos), { streamParser } = this.lang;
        let lookahead = (n) => {
            let pos = this.pos;
            for (let i = 0; i < n; i++) {
                pos += this.input.lineAfter(pos).length;
                if (pos < this.input.length)
                    pos++;
            }
            return this.input.lineAfter(pos);
        };
        let stream = new StringStream(line, this.context ? this.context.state.tabSize : 4, getIndentUnit(this.context.state), lookahead);
        if (stream.eol()) {
            streamParser.blankLine(this.state, stream.indentUnit);
        }
        else {
            let lineClasses = [];
            let chunks = [];
            let last = null;
            while (!stream.eol()) {
                let token = readToken(streamParser.token, stream, this.state);
                if (token) {
                    let classes = token.split(' ').filter(t => {
                        if (!t)
                            return false;
                        let lineClass = t.match(/(?:^|\s+)line-(background-)?(\S+)/);
                        if (lineClass) {
                            lineClasses.push(lineClass[2]);
                            return false;
                        }
                        return true;
                    });
                    let cls = classes.sort().join(' ');
                    let from = this.pos + stream.start;
                    let to = this.pos + stream.pos;
                    if (last && last.cls === cls && last.to === from) {
                        last.to = to;
                    }
                    else {
                        last = { cls, from, to };
                        chunks.push(last);
                    }
                }
            }
            for (let chunk of chunks) {
                this.chunk.push(tokenID(chunk.cls), chunk.from, chunk.to, 4);
            }
            if (lineClasses.length > 0) {
                this.chunk.push(tokenID(lineClasses.join(' '), true), this.pos, this.pos + line.length, (chunks.length + 1) * 4);
            }
        }
        this.pos += line.length;
        if (this.pos < this.input.length)
            this.pos++;
    }
    finishChunk() {
        let tree = Tree.build({
            buffer: this.chunk,
            start: this.chunkStart,
            length: this.pos - this.chunkStart,
            nodeSet,
            topID: 0,
            maxBufferLength: 2048 /* ChunkSize */
        });
        this.lang.stateAfter.set(tree, this.lang.streamParser.copyState(this.state));
        this.chunks.push(tree);
        this.chunkPos.push(this.chunkStart - this.startPos);
        this.chunk = [];
        this.chunkStart = this.pos;
    }
    finish() {
        return new Tree(this.lang.topNode, this.chunks, this.chunkPos, this.pos - this.startPos).balance();
    }
    forceFinish() {
        return this.finish();
    }
}
function readToken(token, stream, state) {
    stream.start = stream.pos;
    for (let i = 0; i < 10; i++) {
        let result = token(stream, state);
        if (stream.pos > stream.start)
            return result;
    }
    throw new Error("Stream parser failed to advance stream.");
}
const tokenTable = Object.create(null);
const typeArray = [NodeType.none];
const nodeSet = new NodeSet(typeArray);
function tokenID(tag, lineMode) {
    return !tag ? 0 : tokenTable[tag] || (tokenTable[tag] = createTokenType(tag, lineMode));
}
function createTokenType(tagStr, lineMode) {
    let name = tagStr.replace(/ /g, "_"), type = NodeType.define({
        id: typeArray.length,
        name,
        props: [lineMode ? lineClassNodeProp.add({ [name]: tagStr }) : tokenClassNodeProp.add({ [name]: tagStr })]
    });
    typeArray.push(type);
    return type.id;
}
function docID(data) {
    let type = NodeType.define({ id: typeArray.length, name: "Document", props: [languageDataProp.add(() => data)] });
    typeArray.push(type);
    return type;
}
// The NodeProp that holds the css class we want to apply
const tokenClassNodeProp = new NodeProp();
const lineClassNodeProp = new NodeProp();
class LineHighlighter {
    constructor(view) {
        this.lineCache = Object.create(null);
        this.tokenCache = Object.create(null);
        this.tree = syntaxTree(view.state);
        this.decorations = this.buildDeco(view);
    }
    update(update) {
        let tree = syntaxTree(update.state);
        if (tree.length < update.view.viewport.to) {
            this.decorations = this.decorations.map(update.changes);
        }
        else if (tree != this.tree || update.viewportChanged) {
            this.tree = tree;
            this.decorations = this.buildDeco(update.view);
        }
    }
    buildDeco(view) {
        if (!this.tree.length)
            return Decoration.none;
        let builder = new RangeSetBuilder();
        let lastPos = 0;
        for (let { from, to } of view.visibleRanges) {
            this.tree.iterate({
                from, to,
                enter: (type, start, end) => {
                    let lineStyle = type.prop(lineClassNodeProp);
                    if (lineStyle) {
                        let lineDeco = this.lineCache[lineStyle] || (this.lineCache[lineStyle] = Decoration.line({ attributes: { class: lineStyle } }));
                        let newFrom = view.visualLineAt(start).from;
                        // Ignore line modes that start from the middle of the line text
                        if (lastPos > newFrom) {
                            return;
                        }
                        builder.add(newFrom, newFrom, lineDeco);
                        lastPos = newFrom;
                    }
                    let tokenStyle = type.prop(tokenClassNodeProp);
                    if (tokenStyle) {
                        let lineDeco = this.tokenCache[tokenStyle] || (this.tokenCache[tokenStyle] = Decoration.mark({ class: tokenStyle.split(' ').map(c => 'cm-' + c).join(' ') }));
                        builder.add(start, end, lineDeco);
                        lastPos = end;
                    }
                }
            });
        }
        return builder.finish();
    }
}
// This extension installs a highlighter that highlights lines based on the node prop above
const lineHighlighter = Prec.fallback(ViewPlugin.define(view => new LineHighlighter(view), {
    decorations: v => v.decorations
}));

export { StreamLanguage, StringStream, lineClassNodeProp, lineHighlighter, tokenClassNodeProp };
