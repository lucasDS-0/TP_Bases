# Introducción

La propuesta realizada es una detector de joins por _primary keys_ . \
El proyecto consiste en un parser hecho en `Haskell` para leer código fuente en `TypeScript`, el cual detecta consultas de `SQL:2011` decoradas con una _hint_ que nos indica información sobre la consulta en sí.
Luego, junto con una base dada por su _DLL_ en un archivo `.json`, corroboramos si dicha conosulta es un `SELECT`, si la misma cuenta con un `JOIN`, y en este caso, poder distinguir gracias la  hint ofrecida si el campo por que el cual sucede el join es una primary key o no, y si esto es informado.  \
El resultado de este análisis, es ofrecido como resultado de la ejecución del programa.

## Motivación

Durante la ejecución del Query Planner de un motor de bases de datos, distintos análisis son realizados. Algunos de ellos involucran decidir si utilizar un índice clustered o unclustered, y parte de esta decisión consiste en saber si una consulta joinea por primary key o no. Este trabajo práctico es un prototipo para realizar esta verificaición.

## Estructura del proyecto

Omitiendo archivos de configuración y metadata, el proyecto consta de la siguiente estructurea

```Shell
├── app/
│   ├── Main.hs
└── src/
    ├── PKDetector.hs
    ├── QueryParser.hs
    ├── HintParser.hs
    ├── DLLParser.hs
    ├── ddl.json
    └── sql_hint.ts
```
Por un lado, en `app/Main.hs` se encuentra la entrada al programa, de este archivo se crea un ejecutable. 
El resto de los modulos tienen cada uno una responsabilidad específica. `src/ddl.json` y `src/sql_hint.ts` son archivos de fallback en caso de no proveer otros al usar el programa, y contienen ejemplos utilizados en este prototipo.

### Funcionamiento de los moódulos

`DLLParser` lee el contenido de `src/ddl.json` y lo embébe en GADTs para utilizar posteriormente. Por ejemplo, el DDL de la tabla empleados es el siguiente

```JSON
{
    "name": "empleados",
    "fields": [
      {
        "field_name": "empleado",
        "field_type": "VARCHAR(30)",
        "not_null": true
      },
      {
        "field_name": "email",
        "field_type": "VARCHAR(64)",
        "not_null": true
      }
    ],
    "fk_list": [],
    "pk":"empleado"
}
```

Por otro lado, pro ejemplo, la siguiente es una consulta presente en `src/sql_hint.ts` con una hint asociada a la misma.

```TypeScript
// sql-hint join( join(alias e, alias a, no-flag), alias t, no-flag)
var sqlExcesoAsignaciones = `
    SELECT empleado, count(*)
        FROM empleados e
            left join asignaciones a using (empleado)
            left join trimestres t using (trimestre)
        GROUP BY count(*)
        HAVING count(*) > 1
    `;
```

`QueryParser` y `HintParser` son reponsables de parsear esta consulta y su hint. Resultanto en los siguientes valores a ser procesados luego:

- Hint
```Haskell
Join (Join (Alias "e") (Alias "a") NoFlag) (Alias "t") NoFlag
```
- Consulta
```Haskell
JoinUsingClause (JoinUsingClause (Table "empleados" "e") (Table "asignaciones" "a") "empleado") (Table "trimestres" "t") "trimestre"
```

Por último, `PKDetector` utiliza la información proveída por el resto de los módulos, y comunica el veredicto de interés.

### Relación Hint-Consulta

En el ejemplo previo, tenemos una consulta acompañada por su hint. Pero para poder utilizar esta información, se parsea a una representación inernada dada por el siguiente tipo
```Haskell
data HintFlag
    = AllowNoPK 
    | NoFlag 
    deriving (Eq, Show)

data SQLHint 
    = Alias Text.Text
    | Name Text.Text
    | Join SQLHint SQLHint HintFlag
    | Malformed TP.ParseError
    deriving (Eq, Show)
```
Donde cada constructor indentifica los posibles casos de interés
- `Alias`: Indentifica una tabla por su alias en la consulta, por ejemplo `Alias "e"` para
  
  ```TypeScript
  // sql-hint alias e
  ```
- `Name`: Indentifica una tabla por el nombre dado en la consulta, por ejemplo `Name "empleados"` para
  
  ```TypeScript
  // sql-hint name e
  ```
- `Join`: Identifica un join entre dos tablas donde las mismas están construidas recursivamente, y una posible flag. Por ejemplo, `Join (Alias "e") (Alias "a") NoFlag` para
  
  ```TypeScript
  // sql-hint join(alias e, alias a, no-flag)
  ```
- `Malformed`: En caso de que cuando se parseo la hint, el parser detectó algún error de sintáxis en su definición. Por ejemplo, `Malformed "unexpected end of input. expecting "name ""` para
  ```TypeScript
  // sql-hint name
  ```
  
Lo importante de esto, es que en cado de no estar joineando a través de una primary key de alguna de las tablas involucradas, la hint que acompaña la consulta debe indicar esto con el flag `AllowNoPK`. Si no lo informa (utiliza `NoFlag`), será indicado al ejecutar el programa. Caso opuesto, si no joinea por PK, el flag no es relevante.

El módulo `PKDetector` se ocupa de verificar que la hint y la consulta compartan la misma estructura.

## Uso

El proyecto provee un archivo de configuración `.cabal` para usar a través de la herramienta Cabal (automatiza la compilación de proyectos, revisa dependencias, gestiona paquetes, etc.). El mismo puede ser instalado a través de [GHCup](https://www.haskell.org/ghcup/).

El proyecto puede compilarse con `cabal build`. Para ejecutar el prorama compilado, existen dos alternativas
- Desde Cabal
```Shell
cabal run exes -- ddlFile.json sourceFile.ts
```
- Con el ejecutable
```Shell
./TP-Bases ddlFile.json sourceFile.ts
```

## Comentarios y Supuestos

- No se utilizó IA en ninguna etapa del desarrollo.
- No se validan contrains propios de la base, _i.e._ el DDL se asume correcto.
- Se trabaja con código fuente en `TypeScript`. Dado que esto es únicamente dependiente de la estructura de comentarios para hints y cadenas de texto para las consultas, se puede modificar fácilmente para contemplar otros lenguajes fuente.
- Solo consultas decoradas por hints son parseadas.
- Se trabaja con `ANSI SQL 2011`.
