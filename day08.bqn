ParseInput ← { '0'-˜(≠𝕩)‿∘⥊∾𝕩 }
testInput ← ParseInput •FLines "inputs/08example.txt"

Part1 ← {
  MarkVisible ← ∊⌈`
  left ← MarkVisible˘ 𝕩
  right ← (MarkVisible⌾⌽)˘ 𝕩
  top ← MarkVisible˘⌾⍉ 𝕩
  bottom ← (MarkVisible⌾⌽)˘⌾⍉ 𝕩

  +´⥊∨´ ⟨left,right,top,bottom⟩
}

realInput ← ParseInput •FLines "inputs/08.txt"
Part1 realInput
