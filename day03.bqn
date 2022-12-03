strings ← •Import "bqn-libs/strings.bqn"
⟨lines⟩ ← •Import "utils.bqn"

Parse ← ¯1↓Lines

exampleInput ← •FChars "inputs/day03Example.txt"
exampleParsed ← Parse exampleInput
input ← •FChars "inputs/day03.txt"
parsed ← Parse input

1 + 'c' - 'a'
26 + 1 + 'B' - 'a' 
'a' - 'A'
Priority ← {
	('a'≤𝕩)∧(𝕩≤'z')? 1+𝕩-'a' ;
	('A'≤𝕩)∧(𝕩≤'Z')? 26+1+𝕩-'A'
}
Priority 'z'
Priority (⊑ "a")
(Priority ⊑) "a"

# for each line:
# - reshape so the two halves become major cells
# - take the intersection of the halves
# - take the sole element and get its priority
# then sum the priority of each line
SolvePart1 ← +´ {Priority ⊑ (∊/⊣)˝ 2‿∘⥊𝕩}¨
SolvePart1 exampleParsed
SolvePart1 parsed
# SolvePart2 ← ·


SolvePart2 exampleInput
SolvePart2 input
