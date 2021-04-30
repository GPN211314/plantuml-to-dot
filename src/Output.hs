module Output (outputPlantUMLAsDot) where

import Grammar

outputPlantUMLAsDot :: [Element] -> String
outputPlantUMLAsDot elements =
  "digraph G {\nnode [shape=record]\n" ++ outputElements elements ++ "}\n"

outputElements = concat . map outputElement

outputElement (Node nodeType name fields methods) =
  outputName name ++ " [label=\"{" ++ outputNodeType nodeType ++ " " ++ name ++
  outputNodeElements fields ++ outputNodeElements methods ++ "}\"]\n"
outputElement (Edge (EdgeEnd leftName leftHead) (EdgeEnd rightName rightHead) style annotation) =
  left ++ right
      where left = case leftHead of
                     "<|" -> outputName rightName ++ " -> " ++ outputName leftName ++ " [arrowhead=onormal,style=" ++ outputEdgeStyle style ++  "]\n"
                     "<"  -> outputName rightName ++ " -> " ++ outputName leftName ++ " [arrowhead=vee,style=" ++ outputEdgeStyle style ++  "]\n"
                     "*"  -> outputName rightName ++ " -> " ++ outputName leftName ++ " [arrowhead=diamond,style=" ++ outputEdgeStyle style ++  "]\n"
                     "o"  -> outputName rightName ++ " -> " ++ outputName leftName ++ " [arrowhead=odiamond,style=" ++ outputEdgeStyle style ++  "]\n"
                     _    -> ""
            right = case rightHead of
                     "|>" -> outputName leftName ++ " -> " ++ outputName rightName ++ " [arrowhead=onormal,style=" ++ outputEdgeStyle style ++  "]\n"
                     ">"  -> outputName leftName ++ " -> " ++ outputName rightName ++ " [arrowhead=vee,style=" ++ outputEdgeStyle style ++  "]\n"
                     "*"  -> outputName leftName ++ " -> " ++ outputName rightName ++ " [arrowhead=diamond,style=" ++ outputEdgeStyle style ++  "]\n"
                     "o"  -> outputName leftName ++ " -> " ++ outputName rightName ++ " [arrowhead=odiamond,style=" ++ outputEdgeStyle style ++  "]\n"
                     _    -> ""


outputNodeType Class = "class"
outputNodeType Interface = "interface"
outputNodeType AbstractClass = "abstract class"
outputNodeType Enum = "enum"

outputName name = "\"" ++ name ++ "\""

outputNodeElements [] = ""
outputNodeElements x = "|" ++ concat (map outputNodeElement x)

outputNodeElement (NodeElement e) = e ++ "\\n"
outputNodeElement (NodeBreak) = "|"

outputEdgeStyle Solid = "solid"
outputEdgeStyle Dotted = "dotted"
