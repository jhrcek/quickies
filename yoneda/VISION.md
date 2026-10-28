An app to explain Yoneda lemma, both visually and using simple proof, including interactive playground to explore all the prerequisite concepts.

Start with compact statement:
"For all C, A in C, F : C -> Set, Nat(Hom(A,_), F) ~= F(A)"

Provide translation to words
"For all categories, for all objects A in category C and for any set valued functor F : C -> Set, the set of natural transformations from the Hom(A,_) to F is isomorphic to F(A)"

From the translation link to concepts + playgrounds to explore them

Concepts to explain (organize them into olog)

Natural transformation
- Hom Functors
    - Set valued functors
        - Functors
            - Categories
                - Object
                - Morphism
                - Composition rule


Interactive features
Category editor
- adding/removing objects
- adding/removing morphisms
    - checks that necessary hom sets are non-empty to allow composing existing morphisms
- editing composition table
    - checks that composition associative; feature to complete filling in existing table - or warn if not possible and why
    - different ways to sort the table