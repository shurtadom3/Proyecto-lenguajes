# Proyecto-lenguajes
Sara Hurtado Metaute y Alyson Henao, REPLIT

import System.IO
import Control.Exception (try, IOException)

-- Definición del tipo de datos
data Articulo = Articulo {
    nombre :: String,
    categoria :: String
  } deriving (Show, Read)

-- Función para registrar entrada de artículo
registrarArticulo :: Articulo -> [Articulo] -> [Articulo]
registrarArticulo articulo inventario = articulo : inventario

-- Función para buscar artículos por categoría
buscarArticulo :: String -> [Articulo] -> IO (Maybe Articulo)
buscarArticulo categoriaBuscada articulos =
    case filter (\x -> categoriaBuscada == categoria x) articulos of
        [] -> return Nothing

        (x:xs) -> do
            putStrLn "Artículos encontrados:"
            mapM_ print (x:xs)
            return (Just x)


--- Función para guardar la información de los artículos en un txt
guardarArticulos :: [Articulo] -> IO ()
guardarArticulos articulos = do
    withFile "inventario.txt" WriteMode $ \handle -> do
        mapM_ (hPutStrLn handle . show) articulos
    

-- Función para cargar la info desde un txt
cargarArticulos :: IO [Articulo]
cargarArticulos = do
    withFile "inventario.txt" ReadMode $ \handle -> do
        contents <- hGetContents handle
        let lineas = lines contents
        putStrLn $ "Artículos cargados: " ++ show (length lineas)
        return (map read lineas)
    

-- Función para mostrar la info de los artículos
mostrarArticulos :: Articulo -> String
mostrarArticulos articulo =
    "Artículo: " ++ nombre articulo ++ ". Categoria: " ++ categoria articulo



-- Función ciclo principal
cicloPrincipal :: [Articulo] -> IO ()
cicloPrincipal articulos = do
    putStrLn "\nSeleccione una opción:"
    putStrLn "1. Registrar entrada de artículo."
    putStrLn "2. Buscar artículos por categoría."
    putStrLn "3. Listar todos los artículos."
    putStrLn "4. Mostrar cantidad de artículos por categoría."
    putStrLn "5. Salir"
    opcion <- getLine

    case opcion of
        "1" -> do
            putStrLn "Ingrese el nombre del artículo:"
            nombreArticulo <- getLine
            putStrLn "Ingrese la categoría del artículo:"
            categoriaArticulo <- getLine
            let nuevoArticulo = Articulo nombreArticulo categoriaArticulo
            let nuevoInventario = registrarArticulo nuevoArticulo articulos
            putStrLn $ "Artículo " ++ nombreArticulo ++ " registrado exitosamente."
            guardarArticulos nuevoInventario
            cicloPrincipal nuevoInventario

        "2" -> do
            putStrLn "Ingrese la categoría a buscar:"
            categoriaBuscada <- getLine
            resultado <- buscarArticulo categoriaBuscada articulos
            case resultado of
                Just articulo -> putStrLn $ "Artículo encontrado: " ++ mostrarArticulos articulo
                Nothing -> putStrLn "No se encontraron artículos con esa categoría."
            cicloPrincipal articulos

        "3" -> do
            putStrLn "Lista de todos los artículos:"
            mapM_ (putStrLn . mostrarArticulos) articulos
            cicloPrincipal articulos

        "4" -> do
            putStrLn "Ingrese la categoría para encontrar la cantidad de artículos:"
            categoriaBuscada <- getLine
            let cantidad = length (filter (\x -> categoriaBuscada == categoria x) articulos)
            putStrLn $ "Hay " ++ show cantidad ++ " artículos en la categoría " ++ categoriaBuscada
            cicloPrincipal articulos

        "5" -> putStrLn "¡Hasta luego!"
        

        _ -> do
            putStrLn "Opción inválida. Intente nuevamente."
            cicloPrincipal articulos

-- Función principal
main :: IO ()
main = do
    -- Carga de los artículos
    articulos <- cargarArticulos
    putStrLn "¡Bienvenido al sistema de gestión de inventario!"

    -- Ciclo principal
    cicloPrincipal articulos
