strings ← •Import "bqn-libs/strings.bqn"
⟨Paragraphs⟩ ← •Import "utils.bqn"

Parse ← strings.ToNats

exampleInput ← •FChars "inputs/day0NExample.txt"
exampleParsed ← Parse exampleInput

input ← •FChars "inputs/day0N.txt"
parsed ← Parse input

SolvePart1 ← ·
SolvePart2 ← ·

SolvePart1 exampleInput
SolvePart1 input

SolvePart2 exampleInput
SolvePart2 input
