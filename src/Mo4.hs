module Mo4 (javaRecipe, Recipe (..)) where

type RecipeName = [Char]
type Ingredients = [[Char]]

data Recipe = Recipe { 
                       recipeName  :: RecipeName
		     , ingredients :: Ingredients
		     } deriving (Show, Eq)

printWM :: [Char]
printWM = "Mo4"

javaRecipe :: Recipe
javaRecipe = Recipe "Java" ["Water", "JVM", "JRE", "JDK", "OpenJDK", "OpenJDKjk", "Maven", "Gradle", "Cradle", "Ladle", "JUnit", "Coffee", "ptsd::endl"]

