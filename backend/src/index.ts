import Parser, { SyntaxNode } from "tree-sitter";
import { spawn } from "child_process";
import JavaScript from "../parsers/tree-sitter-javascript";
import Elm from "../parsers/tree-sitter-elm";
import Json from "../parsers/tree-sitter-json";
import express from "express";
import BodyParser from "body-parser";
import cors from "cors";
const Query: any = (Parser as any).Query;

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

const testQuery = (
  languageName: string,
  source: string,
  queryString: string
) => {
  const parser = new Parser();
  const language = getLanguage(languageName);
  parser.setLanguage(language);
  const tree = parser.parse(source);
  console.log(tree.rootNode.toString());
  const query = new Query(language, queryString);
  const captures = query.captures(tree.rootNode);
  return captures;
};

const parse = (language: string, source: string) => {
  const parser = new Parser();
  parser.setLanguage(getLanguage(language));
  const tree = parser.parse(source);
  console.log(tree.rootNode.toString());
  console.log();

  console.log(JSON.stringify(toAst(tree.rootNode), null, 2));

  return tree;
};

interface Ast {
  tag: string;
  text?: string;
  children?: Ast[];
}

const toAst = (syntaxNode: SyntaxNode): Ast =>
  syntaxNode.childCount === 0
    ? { tag: syntaxNode.type, text: syntaxNode.text }
    : {
        tag: syntaxNode.type,
        children: syntaxNode.namedChildren.map(toAst),
      };

const source = `x =  {a = a.b}`;

const queryString = `
(module_declaration (upper_case_qid) @module) 

(import_clause (upper_case_qid) @import)

(exposed_type (upper_case_identifier) @type)
(type_declaration (upper_case_identifier) @type)
(type_alias_declaration (upper_case_identifier) @type)

(function_declaration_left (lower_case_identifier) @value)
(exposed_value) @value
`;

const executeInTalon = (statement: string) => {
  const cmd = `echo "${statement}" | ~/.talon/.venv/bin/repl`;
  spawn("sh", ["-c", cmd]);
};

// // executeInTalon("user.knausj_talon.lsp.x = '3'");
// const captures = testQuery("elm", source, queryString);
// console.log(captures);
// const data = captures.map((e) => {
//   return { name: e.name, text: e.node.text };
// });
// executeInTalon(`user.knausj_talon.lsp.x = '${JSON.stringify(data)}'`);
// data.forEach((e) => {
//   console.log(e);
// });

export const startServer = () => {
  const app = express();
  const port = 8081; // default port to listen

  app.use(BodyParser.json());
  app.use(cors());

  app.get("/", (req, res) => {
    const tree = parse("elm", source);
    const ast = toAst(tree.rootNode);
    res.send(JSON.stringify(ast));
  });

  // define a route handler for the default home page
  app.post("/", (req, res) => {
    const language = req.body.language;
    const source = req.body.source;
    console.log(language, source);
    const tree = parse(language, source);
    const ast = toAst(tree.rootNode);
    res.send(JSON.stringify(ast));
  });

  // start the Express server
  app.listen(port, () => {
    console.log(`server started at http://localhost:${port}`);
  });
};

startServer();

/*
 * type Ast = Tree.Tree Node'
data Node' = Node' {type_::String, text :: String} deriving Show

toAst :: Node -> IO Ast
toAst Node {..} = do
  -- Node {..} <- peek n
  theType <- peekCString nodeType
  let childCount = fromIntegral nodeChildCount
  children <- mallocArray childCount
  tsNode   <- malloc
  poke                     tsNode nodeTSNode
  ts_node_copy_child_nodes tsNode children
  text      <- ts_node_string_p tsNode >>= peekCString
  children' <- forM [0 .. childCount - 1] $ \n -> do
    child <- peekElemOff children n
    toAst child

  return $ Tree.Node (Node' theType text) children'


printChildren :: Ptr Node -> Int -> IO ()
printChildren children count = forM_
  [0 .. count - 1]
  ( \n -> do
    child <- peekElemOff children n
    printNode child
  )

printNode :: Node -> IO ()
printNode n@Node {..} = do
  theType <- peekCString nodeType
  let TSPoint {..} = nodeStartPoint n
      start        = "(" ++ show pointRow ++ "," ++ show pointColumn ++ ")"
  let TSPoint {..} = nodeEndPoint
      end          = "(" ++ show pointRow ++ "," ++ show pointColumn ++ ")"
  print $ theType ++ start ++ "-" ++ end
*/
