module Mo4 (javaRecipe, extractRecipeName, extractIngredients) where

type RecipeName = [Char]
type Ingredients = [[Char]]

data Recipe = Recipe RecipeName Ingredients
              deriving (Show)

printWM :: [Char]
printWM = "Mo4"

javaRecipe :: Recipe
javaRecipe = Recipe "Java" ["Water", "JVM", "JRE", "JDK", "OpenJDK", "OpenJDKjk", "Maven", "Gradle", "Cradle", "Ladle", "JUnit", "Coffee", "ptsd::endl"]

extractRecipeName :: Recipe -> RecipeName
extractRecipeName (Recipe recipeName _) = recipeName

extractIngredients :: Recipe -> Ingredients
extractIngredients (Recipe _ ingredients) = ingredients
