strings ← •Import "bqn-libs/strings.bqn"
⟨Paragraphs⟩ ← •Import "utils.bqn"

Parse ← strings.ToNats¨ Paragraphs
input ← •FChars "inputs/day01.txt"
parsed ← Parse input

SolvePart1 ← ⌈´+´¨
SolvePart2 ← +´3↑⟜∨+´¨

SolvePart1 parsed
SolvePart2 parsed
