# TODO

- [ ] generate random permutation of specific type (e.g. transposition, involution etc.)
- [ ] enumerate permutations by index (to allow "next" / "previous" permutation)
- [ ] turn it into application with url parsing (e.g./n/5/compose/p/...(some encoding).../q/...(some encoding)...)
- [ ] add concept lattice with all the boolean properties, showing number of permutations belonging to each node for given S_n
    - [ ] maybe use the lattice to structure characteristics view of boolean properties

## Permutation characteristics to show

### Cycle-Based Properties
- [x] 1. Cycle type - partition of n by cycle lengths, e.g., [3, 2, 1] for (0 1 2)(3 4)(5)
- [x] 2. Number of cycles - including fixed points
- [x] 3. Number of fixed points - elements where σ(i) = i
- [x] 4. Order - smallest k where σᵏ = identity (LCM of cycle lengths)
- [x] 5. Sign/parity - even (+1) or odd (-1); equals (-1)^(n - #cycles)

### Boolean Classifications
- [x] 6. Is identity - all fixed points
- [x] 7. Is transposition - exactly one 2-cycle, rest fixed
- [x] 8. Is involution - σ² = identity (all cycles have length ≤ 2)
- [x] 9. Is derangement - no fixed points
- [x] 10. Is cyclic - consists of a single n-cycle

### Inversion-Based Statistics
- [ ] 11. Number of inversions - pairs (i,j) where i < j but σ(i) > σ(j)
- [ ] 12. Descent set - positions i where σ(i) > σ(i+1)
- [ ] 13. Number of descents - size of descent set
- [ ] 14. Major index - sum of descent positions
- [ ] 15. Lehmer code - vector where position i counts inversions involving i

### Excedance Statistics
- [ ] 16. Excedances - positions where σ(i) > i
- [ ] 17. Weak excedances - positions where σ(i) ≥ i
- [ ] 18. Anti-excedances - positions where σ(i) < i

### Group-Theoretic
- [x] 19. Centralizer size - # of permutations commuting with σ (formula: ∏ (kᵢ! · mᵢ^kᵢ) where kᵢ cycles of length mᵢ)
- [x] 20. Conjugacy class size - n! / centralizer size
