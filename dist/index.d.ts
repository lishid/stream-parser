import { Extension } from '@codemirror/state';
import { NodeProp } from 'lezer-tree';
import { IndentContext, Language } from '@codemirror/language';

declare class StringStream {
    string: string;
    private tabSize;
    indentUnit: number;
    lookAhead: (n: number) => string;
    pos: number;
    start: number;
    private lastColumnPos;
    private lastColumnValue;
    eol(): boolean;
    sol(): boolean;
    peek(): string | undefined;
    next(): string | void;
    eat(match: string | RegExp | ((ch: string) => boolean)): string | void;
    eatWhile(match: string | RegExp | ((ch: string) => boolean)): boolean;
    eatSpace(): boolean;
    skipToEnd(): void;
    skipTo(ch: string): boolean | void;
    backUp(n: number): void;
    column(): number;
    indentation(): number;
    match(pattern: string | RegExp, consume?: boolean, caseInsensitive?: boolean): boolean | RegExpMatchArray | null;
    current(): string;
}

interface StreamParser<State> {
    token(stream: StringStream, state: State): string | null;
    blankLine?(state: State, indentUnit: number): void;
    startState?(indentUnit: number): State;
    copyState?(state: State): State;
    indent?(state: State, textAfter: string, context: IndentContext): number | null;
    languageData?: {
        [name: string]: any;
    };
}
declare class StreamLanguage<State> extends Language {
    private constructor();
    static define<State>(spec: StreamParser<State>): StreamLanguage<State>;
    private getIndent;
    get allowsNesting(): boolean;
}
declare const tokenClassNodeProp: NodeProp<string>;
declare const lineClassNodeProp: NodeProp<string>;
declare const lineHighlighter: Extension;

export { StreamLanguage, StreamParser, StringStream, lineClassNodeProp, lineHighlighter, tokenClassNodeProp };
