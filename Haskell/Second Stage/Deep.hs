module Deep where

import Shallow (Point, Region, Transformation)
import qualified Shallow as S

{-
    Deep embeddings pentru regiuni și transformări. Fiecare regiune
    și transformare este reprezentată sub forma unui arbore sintactic
    (abstract syntax tree, AST) ce descrie secvența de operații care contribuie
    la construcția acelei regiuni sau transformări. De exemplu, expresia
    (circles 2), unde circles a fost definită în etapa 1, ar produce acum
    un rezultat similar cu

    Union (Circle 2.0) (Transform (Translation 6.0 0.0) (Circle 2.0)).

    Pentru a obține acest efect, toate funcțiile din etapa 1 sunt reimplementate
    astfel încât să utilizeze direct constructorul de date potrivit al unui
    tip de date. De exemplu, funcția fromPoints *este* acum constructorul
    FromPoints.

    Primul avantaj major al reprezentării bazate pe AST-uri este posibilitatea
    interpretării acesteia în diverse moduri pentru a reconstitui semnificații
    concrete variate ale regiunilor și transformărilor, e.g. regiuni ca funcții
    caracteristice, și transformări ca funcții pe puncte, ca în etapa 1.
    Vom vedea și alte semnificații concrete în etapa 3.

    Al doilea mare avantaj îl constituie posibilitatea simplificării AST-ului
    înaintea interpretării lui într-o manieră specifică. Observați deja cum
    funcțiile combineTransformations și applyTransformation de mai jos, văzute
    ca smart constructors, recunosc anumite cazuri particulare și simplifică
    AST-ul încă de la construcție.
-}
data RegionAST
    = FromPoints [Point]
    | Rectangle Float Float
    | Circle Float
    | Complement RegionAST
    | Union RegionAST RegionAST
    | Intersection RegionAST RegionAST
    | Transform TransformationAST RegionAST
    deriving (Show, Eq)

data TransformationAST
    = Translation Float Float
    | Scaling Float
    | Combine [TransformationAST]
    deriving (Show, Eq)

fromPoints :: [Point] -> RegionAST
fromPoints = FromPoints

rectangle :: Float -> Float -> RegionAST
rectangle = Rectangle

circle :: Float -> RegionAST
circle = Circle

complement :: RegionAST -> RegionAST
complement = Complement

union :: RegionAST -> RegionAST -> RegionAST
union = Union

intersection :: RegionAST -> RegionAST -> RegionAST
intersection = Intersection

translation :: Float -> Float -> TransformationAST
translation = Translation

scaling :: Float -> TransformationAST
scaling = Scaling

{-
    Smart constructor: dacă lista de transformări este singleton, înlocuiește
    lista cu unica transformare din listă; altfel, utilizează constructorul
    de date Combine.
-}
combineTransformations :: [TransformationAST] -> TransformationAST
combineTransformations [transformation] = transformation
combineTransformations transformations = Combine transformations

{-
    Smart constructor: dacă se dorește aplicarea unei liste vide de transformări
    asupra unei regiuni, întoarce regiunea ca atare; altfel, utilizează
    constructorul de date Transform.
-}
applyTransformation :: TransformationAST -> RegionAST -> RegionAST
applyTransformation (Combine []) region = region
applyTransformation transformation region = Transform transformation region

{-
    *** TODO ***

    Implementați funcția toTransformation, care constituie o interpretare
    a AST-ului unei transformări (TransformationAST), în vederea recuperării
    reprezentării concrete din etapa 1, sub forma unei funcții cu tipul
    Transformation = (Point -> Point).

    Nu este necesar să implementați de la zero, ci puteți invoca direct
    funcțiile din etapa 1, prefixându-le cu "S." (observați la începutul acestui
    fișier linia "import qualified Shallow as S"). Mai precis, funcțiile
    neprefixate, e.g. translation, sunt cele din acest fișier, iar funcțiile
    prefixate, e.g. S.translation, sunt cele din modulul Shallow.
-}
toTransformation :: TransformationAST -> Transformation
toTransformation transformation = 
    case transformation of
        Translation x y -> S.translation x y
        Scaling x -> S.scaling x
        Combine transformations -> S.combineTransformations $ map toTransformation transformations

{-
    *** TODO ***

    Implementați funcția toRegion, care constituie o interpretare a AST-ului
    unei regiuni (RegionAST), în vederea recuperării reprezentării concrete
    din etapa 1, sub forma unei funcții caracteristice cu tipul
    Region = (Point -> Bool).
-}
toRegion :: RegionAST -> Region
toRegion region = 
    case region of
        FromPoints points -> S.fromPoints points
        Rectangle x y -> S.rectangle x y
        Circle r -> S.circle r
        Complement region -> S.complement $ toRegion region
        Union region1 region2 -> S.union (toRegion region1) (toRegion region2)
        Intersection region1 region2 -> S.intersection (toRegion region1) (toRegion region2)
        Transform transformation region -> S.applyTransformation (toTransformation transformation) (toRegion region)

{-
    Varianta actualizată a a funcției inside.
-}
inside :: Point -> RegionAST -> Bool
inside = flip toRegion

{-
    *** TODO ***

    Implementați funcția decomposeTransformation, care descompune o transformare
    oricât de complexă într-o listă de transformări elementare (translații
    și scalări), conservând bineînțeles ordinea acestora.

    Constrângeri: evitați recursivitatea explicită.

    Hint: valorificați ordinea de sus în jos de realizare a pattern matching-ului,
    pentru a scurta descrierea cazurilor.

    Exemple:

    > decomposeTransformation $ Translation 1 2
    [Translation 1.0 2.0]

    > decomposeTransformation $ Scaling 2
    [Scaling 2.0]

    > decomposeTransformation $
        Combine [ Translation 1 2
                , Combine [ Translation 3 4
                          , Scaling 2
                          ]
                , Scaling 3
                ]
    [Translation 1.0 2.0,Translation 3.0 4.0,Scaling 2.0,Scaling 3.0]
-}
decomposeTransformation :: TransformationAST -> [TransformationAST]
decomposeTransformation transformation = 
    case transformation of
        Translation x y -> [Translation x y]
        Scaling x -> [Scaling x]
        Combine transformations -> concatMap decomposeTransformation transformations

{-
    *** TODO ***

    Implementați funcția fuseTransformations, care alipește transformările
    adiacente de același fel (translații cu translații și scalări cu scalări)
    dintr-o listă de transformări elementare (translații și scalări),
    și întoarce lista transformărilor rezultante.

    Constrângeri: evitați recursivitatea explicită.

    > fuseTransformations [Translation 1 2]
    [Translation 1.0 2.0]

    > fuseTransformations [Scaling 2, Scaling 3]             
    [Scaling 6.0]

    > fuseTransformations [ Translation 1 2, Translation 3 4
                          , Scaling 2, Scaling 3
                          , Translation 5 6
                          ]
    [Translation 4.0 6.0,Scaling 6.0,Translation 5.0 6.0]
-}
fuseTransformations :: [TransformationAST] -> [TransformationAST]
fuseTransformations = foldr f []
    where
        f (Translation x1 y1) (Translation x2 y2 : ts) = Translation (x1 + x2) (y1 + y2) : ts
        f (Scaling x1) (Scaling x2 : ts) = Scaling (x1 * x2) : ts
        f transformation transformations = transformation : transformations

{-
    *** TODO ***

    Implementați funcția optimizeTransformations, care simplifică toate
    transformările din AST-ul unei regiuni. Principiile sunt următoarele:

    * Transformările succesive trebuie descompuse și alipite.
    * Pentru a evidenția lanțuri cât mai lungi de transformări succesive,
      se urmărește deplasarea în sus a transformărilor din AST, astfel:
      * Complementul unei transformări este transformarea complementului.
      * O ramificare (reuniune sau intersecție) de transformări de regiuni
        presupune determinarea celui mai lung prefix de transformări comune
        de pe cele două ramuri și deplasarea acestuia deasupra ramificării,
        păstrând sub ea sufixele de transformări necomune.
    * O regiune elementară (din puncte, dreptunghi sau cerc) se consideră
      optimizată.
    * Toate cosmetizările de mai sus se realizează după optimizarea recursivă
      a subregiunilor.
    
    Constrângeri: evitați duplicarea codului.

    Hints:

    * case pentru pattern matching în interiorul altor expresii
    * smart constructors: combineTransformation, applyTransformation

    Exemple:

    > optimizeTransformations $
        Transform (Combine [ Translation 1 2
                           , Combine [ Translation 3 4
                                     , Scaling 2
                                     ]  
                           , Scaling 3
                           ])
                  (Circle 5)
    Transform (Combine [Translation 4.0 6.0,Scaling 6.0]) (Circle 5.0)

    > optimizeTransformations $
        Transform (Combine [ Translation 1 2
                           , Combine [ Translation 3 4
                                     , Scaling 2
                                     ]  
                           , Scaling 3
                           ])
                  (Transform (Scaling 4)
                             (Transform (Scaling 2) (Circle 5)))
    Transform (Combine [Translation 4.0 6.0,Scaling 48.0]) (Circle 5.0)

    > optimizeTransformations $
        Complement (Transform (Scaling 4)
                              (Transform (Scaling 2) (Circle 5)))
    Transform (Scaling 8.0) (Complement (Circle 5.0))

    > optimizeTransformations $
        Transform (Combine [ Translation 1 2
                           , Combine [ Translation 3 4
                                     , Scaling 2
                                     ]  
                           , Scaling 3
                           ])
                  (Complement (Transform (Scaling 4)
                                         (Transform (Scaling 2) (Circle 5))))
    Transform (Combine [Translation 4.0 6.0,Scaling 48.0]) (Complement (Circle 5.0))

    > optimizeTransformations $
        Union (Complement (Transform (Scaling 4)
                                     (Transform (Scaling 2) (Circle 5))))
              (Rectangle 6 7)
    Union (Transform (Scaling 8.0) (Complement (Circle 5.0))) (Rectangle 6.0 7.0)

    > optimizeTransformations $
        Union (Transform (Combine [ Translation 1 2
                                  , Combine [ Translation 3 4
                                            , Scaling 2
                                            ]  
                                  , Scaling 3
                                  ])
                         (Complement (Transform (Scaling 4)
                                                (Transform (Scaling 2) (Circle 5)))))
              (Transform (Translation 4 6) (Rectangle 6 7))
    Transform (Translation 4.0 6.0)
              (Union (Transform (Scaling 48.0) (Complement (Circle 5.0)))
                     (Rectangle 6.0 7.0))
-}

longestCommonPrefix combineFunction region1 region2 =
    let
        region1' = optimizeTransformations region1
        region2' = optimizeTransformations region2
    in case (region1', region2') of
        (Transform transformation1 region1'', Transform transformation2 region2'') -> 
            let
                -- decompose transformations
                transformations1 = decomposeTransformation transformation1
                transformations2 = decomposeTransformation transformation2

                -- find common transformations
                commonTransformations = takeWhile (\(t1, t2) -> t1 == t2) $ zip transformations1 transformations2

                -- separate common transformations from the rest
                restTransformations1 = drop (length commonTransformations) transformations1
                restTransformations2 = drop (length commonTransformations) transformations2

                -- combine common transformations
                commonTransformations' = combineTransformations $ map fst commonTransformations

                -- apply rest transformations
                restTransformations1' = combineTransformations restTransformations1
                restTransformations2' = combineTransformations restTransformations2
            in
                let 
                    -- test if we have a null transformation
                    testNull = commonTransformations' == Combine []
                    combinedRegion = case testNull of
                                        True -> combineFunction region1' region2'
                                        False -> Transform commonTransformations' (combineFunction (applyTransformation restTransformations1' region1'') (applyTransformation restTransformations2' region2''))
                in
                    combinedRegion
        _ -> combineFunction region1' region2'

optimizeTransformations region = 
    case region of
        -- base cases that don't need optimization
        FromPoints points -> FromPoints points
        Rectangle x y -> Rectangle x y
        Circle r -> Circle r
        
        -- optimize complement region
        Complement region -> 
            let 
                region1 = optimizeTransformations region
            in case region1 of
                Transform transformation region2 -> Transform transformation (Complement region2)
                _ -> Complement region1

        -- optimize union and intersection regions based on the longest common prefix function that gets a union/intersaction function, respectively
        Union region1 region2 -> longestCommonPrefix Union region1 region2
        
        Intersection region1 region2 -> longestCommonPrefix Intersection region1 region2

        Transform transformation region -> 
            let 
                region1 = optimizeTransformations region
            in case transformation of
                Translation x1 y1 ->
                    case region1 of
                        Transform (Translation x2 y2) region2 -> Transform (Translation (x1 + x2) (y1 + y2)) region2
                        Transform (Combine transformations) region2 -> Transform (combineTransformations $ fuseTransformations $ decomposeTransformation $ Combine (Translation x1 y1 : transformations)) region2
                        _ -> Transform (Translation x1 y1) region1

                Scaling x1 -> 
                    case region1 of
                        Transform (Scaling x2) region2 -> Transform (Scaling (x1 * x2)) region2
                        Transform (Combine transformations) region2 -> Transform (combineTransformations $ fuseTransformations $ decomposeTransformation $ Combine (Scaling x1 : transformations)) region2
                        _ -> Transform (Scaling x1) region1

                Combine transformations ->
                    case region1 of
                        Transform transformation region2 -> Transform (combineTransformations $ fuseTransformations $ decomposeTransformation $ Combine (transformations ++ [transformation])) region2
                        _ -> Transform (combineTransformations $ fuseTransformations $ decomposeTransformation $ Combine transformations) region1