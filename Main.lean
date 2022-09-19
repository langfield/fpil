import Fpil


-- Section 1.1

#eval 42 + 19 -- 61 : Nat
#eval String.append "A" (String.append "B" "C") -- "ABC" : String
#eval String.append (String.append "A" "B") "C" -- "ABC" : String
#eval if 3 == 3 then 5 else 7 -- 5 : Nat
#eval if 3 == 4 then "equal" else "not equal" -- "not equal" : String


-- Section 1.3

def joinStringsWith (sep : String) (a : String) (b : String) : String :=
  a ++ sep ++ b

#check joinStringsWith                          -- String → String → String → String
#eval joinStringsWith " " "a" "b"               -- "a b"
#eval joinStringsWith ", " "one" "and another"  -- "one, and another"
#check joinStringsWith ", "                     -- String → String → String

def vol (x : Nat) (y : Nat) (z : Nat) : Nat := x * y * z

#eval vol 1 2 3 -- 6


-- Section 1.4

structure RectangularPrism where
  height : Float
  width : Float
  depth : Float
deriving Repr

def samplePrism : RectangularPrism := {height := 1, width := 1, depth := 1}
#check samplePrism
#eval samplePrism

def nontrivialPrism : RectangularPrism := {height := 2, width := 3, depth := 4}
#check nontrivialPrism
#eval nontrivialPrism

-- Can we use a more expressive type that `Float`?
def badPrism : RectangularPrism := {height := -1, width := 1, depth := 1}
#check badPrism
#eval badPrism

def volume (prism : RectangularPrism) : Float :=
  prism.height * prism.width * prism.depth

#eval volume nontrivialPrism

-- Q: Define a structure named `Segment` that represents a line segment by its
-- endpoints, and define a function `length : Segment → Float` that computes
-- the length of a line segment. `Segment` should have at most two fields. 
structure Point where
  x : Float
  y : Float
deriving Repr

structure Segment where
  a : Point
  b : Point
deriving Repr

def length (s : Segment) : Float :=
  Float.sqrt (Float.pow dx 2 + Float.pow dy 2) where
    dx := s.b.x - s.a.x
    dy := s.b.y - s.a.y

-- Does `length` have the expected type?
#check length

def origin : Point := {x := 0, y := 0}
def p : Point := {x := 3, y := 5}

def s : Segment := Segment.mk origin p
#eval length s -- 5.830952
#eval Float.pow 5.830952 2

-- Q: What names are introduced by the declaration of `RectangularPrism`?

-- The name `.mk` is introduced by the `RectangularPrism` definition.
#eval RectangularPrism.mk 1.0 1.0 1.0

-- Additionally, accessor functions for each field are introduced. Explicitly,
-- these are, e.g. `RectangularPrism.height`.
#eval RectangularPrism.height nontrivialPrism

-- They can also be used via dot-accessor notation directly on values of type
-- `RectangularPrism`.
#eval nontrivialPrism.height
#eval nontrivialPrism.width
#eval nontrivialPrism.depth


-- Q: Which names are introduced by the following declarations of `Hamster` and
-- `Book`? What are their types?

structure Hamster where
  name : String
  fluffy : Bool
deriving Repr

structure Book where
  makeBook ::
  title : String
  author : String
  price : Float
deriving Repr

-- The `Hamster.mk` name is introduced, which has type `String → Bool → Hamster`.
#eval ({ name := "George", fluffy := False } : Hamster)
#eval Hamster.mk "Paul" False
#check Hamster.mk

-- The field accessors for `name` and `fluffy` are introduced, which haves types:
-- * Hamster.name : Hamster → String
-- * Hamster.fluffy : Hamster → Bool
#check Hamster.name
#check Hamster.fluffy

-- For `Book` we see the `::` double colon notation, which is used to override
-- a structure's constructor name. Ordinarily it would create `Book.mk`, but
-- instead, we have `Book.makeBook`.
#check Book.makeBook
#check_failure Book.mk

-- We still have field accessors for `Book`.
#check Book.title
#check Book.author
#check Book.price


-- Section 1.6


-- Experimentation
-- ===============
-- An n-dimensionl analogue of `Segment`.
inductive Vector (α : Type u) : Nat → Type u
  | nil : Vector α 0
  | cons : α → Vector α n → Vector α (n+1)
deriving Repr

def FloatVector := @Subtype (List Float) λ v => true

#check Vector.cons 0 Vector.nil

def listToVector (xs : List α) : Vector α xs.length :=
  sorry

def vectorToList (v : Vector α n) : List α :=
  sorry

structure NSegment (n : Nat) where
  a : Vector Float n
  b : Vector Float n
deriving Repr

def nlength {n : Nat} (s : NSegment n) : Float :=
  sorry

def concat3 (a : String) (b : String) (c : String) : String :=
  a ++ b ++ c

def concat3Variant1 : String → String → String → String := λ a b c => a ++ b ++ c

def concat3Variant2 : (d : String) → (e : String) → (f : String) → String := λ a b c => a ++ b ++ c

#eval concat3Variant2 "a" "b" "c"


def main : IO Unit :=
  IO.println s!"Hello, {hello}!"
