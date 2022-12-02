strings ← •Import "bqn-libs/strings.bqn"
⟨Paragraphs⟩ ← •Import "utils.bqn"

Parse ← ({' '≢𝕩}¨⊸/)¨

3 | 1 - 3
3 | 2 - 1
3 | 3 - 2
Outcome ← { a‿b : 3×3|1+3|a-b}
Outcome 1‿1
# rock paper
Outcome 1‿2
# rock scissors
Outcome 1‿3
Score ← {a‿b: 

∊⟜"ABC"⌾<'A'
ShapeValue ← { 
	∊⟜"ABC"⌾<𝕩 ? (𝕩-@)-64; 
  ∊⟜"XYZ"⌾<𝕩 ? (𝕩-@)-87
}
ShapeValue¨ "ABCXYZ"

("ABC" - @) - 64
("XYZ" - @) - 87

exampleInput ← •FLines "inputs/day02Example.txt"
exampleParsed ← Parse exampleInput

input ← •FLines "inputs/day02.txt"
parsed ← Parse input

# ShapeScore ← { 𝕩 ∊ "ABC"

SolvePart1 ← ·
SolvePart2 ← ·

SolvePart1 exampleInput
SolvePart1 input

SolvePart2 exampleInput
SolvePart2 input
