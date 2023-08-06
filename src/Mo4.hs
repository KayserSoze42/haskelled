module Mo4 (javaRecipe) where

type RecipeName = [Char]
type Ingredients = [[Char]]

data Recipe = Recipe RecipeName Ingredients
              deriving (Show)

printWM :: [Char]
printWM = "Mo4"

javaRecipe :: Recipe
javaRecipe = Recipe "Java" ["Water", "JVM", "JRE", "JDK", "OpenJDK", "OpenJDKjk", "Maven", "Gradle", "JUnit", "Coffee"]

