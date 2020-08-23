import Parser, { SyntaxNode } from "tree-sitter";
import JavaScript from "../parsers/tree-sitter-javascript";
import Elm from "../parsers/tree-sitter-elm";
import Json from "../parsers/tree-sitter-json";
import express from "express";
import BodyParser from "body-parser";
import cors from "cors";

const getLanguage = (language: string) => {
  switch (language) {
    case "json":
      return Json;
    case "elm":
      return Elm;
    case "js":
      return JavaScript;

    default:
      throw "Unknown language";
  }
};

const parse = (language: string, source: string) => {
  const parser = new Parser();
  parser.setLanguage(getLanguage(language));
  const tree = parser.parse(source);
  console.log(tree.rootNode.toString());
  console.log();

  console.log(JSON.stringify(toAst(source, tree.rootNode), null, 2));

  return tree;
};

interface Ast {
  tag: string;
  text?: string;
  children?: Ast[];
}

const toAst = (source: string, syntaxNode: SyntaxNode): Ast =>
  syntaxNode.childCount === 0
    ? { tag: syntaxNode.type, text: syntaxNode.text }
    : {
        tag: syntaxNode.type,
        children: syntaxNode.namedChildren.map((e) => toAst(source, e)),
      };

const source = `
1`;

const sourceCode = `
x a = 5
`;
// parse("elm", source);
parse("json", "1");
// parse(JavaScript, sourceCode);

const app = express();
const port = 8081; // default port to listen

app.use(BodyParser.json());
app.use(cors());

app.get("/", (req, res) => {
  const tree = parse("elm", source);
  const ast = toAst(source, tree.rootNode);
  res.send(JSON.stringify(ast));
});

// define a route handler for the default home page
app.post("/", (req, res) => {
  const language = req.body.language;
  const source = req.body.source;
  console.log(language, source);
  const tree = parse(language, source);
  const ast = toAst(source, tree.rootNode);
  res.send(JSON.stringify(ast));
});

// start the Express server
app.listen(port, () => {
  console.log(`server started at http://localhost:${port}`);
});
