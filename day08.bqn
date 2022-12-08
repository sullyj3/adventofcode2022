ParseInput ← { '0'-˜(≠𝕩)‿∘⥊∾𝕩 }
testInput ← ParseInput •FLines "inputs/08example.txt"
VisibleMask ← ∊ ⌈`

Part1 ← {
  left ← VisibleMask˘ 𝕩
  right ← (⌽ ∘ VisibleMask ∘ ⌽)˘ 𝕩
  top ← ⍉ VisibleMask˘ ⍉ 𝕩
  bottom ← ⍉ (⌽ ∘ VisibleMask ∘ ⌽)˘ ⍉ 𝕩

  +´⥊∨´ ⟨left,right,top,bottom⟩
}

Part1 testInput
realInput ← ParseInput •FLines "inputs/08.txt"
Part1 realInput
