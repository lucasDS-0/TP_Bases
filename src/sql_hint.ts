
// sql-hint 
var sqlExcesoAsignaciones = `
    SELECT empleado, count(*)
        FROM empleados
    `;

// sql-hint name
var sqlExcesoAsignaciones = `
    SELECT empleado, count(*)
        FROM empleados
    `;

// sql-hint name asignaciones
var sqlExcesoAsignaciones = `
    SELECT empleado, count(*)
        FROM empleados
    `;

// sql-hint alias empleados
var sqlExcesoAsignaciones = `
    SELECT empleado, count(*)
        FROM empleados
    `;

// sql-hint name empleados
var sqlExcesoAsignaciones = `
    SELECT empleado, count(*)
        FROM empleados e
    `;

// sql-hint name empleados
var sqlExcesoAsignaciones = `
    SELECT empleado, count(*)
        FROM empleados
    `;

asd
asdasd
asd



// sql-hint join( join(alias e, alias a, no-flag), alias t, no-flag)
var sqlExcesoAsignaciones = `
    SELECT empleado, count(*)
        FROM empleados e
            left join asignaciones a using (empleado)
            left join trimestres t using (trimestre)
        GROUP BY count(*)
        HAVING count(*) > 1
    `;

asdasda
asdasdaasd
asd

asd
asdasd
asd

// sql-hint join( join(alias e, alias a, no-flag), alias t, no-flag)
var sqlExcesoAsignaciones = `
    SELECT empleado, count(*)
        FROM empleados e
            left join asignaciones a using (empleado)
            left join trimestres t using (desc)
        GROUP BY count(*)
        HAVING count(*) > 1
    `;

// sql-hint join( join(alias e, alias a, no-flag), alias t, no-flag)
var sqlExcesoAsignaciones = `
    SELECT empleado, count(*)
        FROM empleados e
            left join asignaciones a using (empleado)
            left join trimestres t using (trimestre)
            left join materia m using (id)
        GROUP BY count(*)
        HAVING count(*) > 1
    `;



// sql-hint join(join(alias e, alias a, allow-no-pk), alias t, allow-no-pk)
var sqlExcesoAsignaciones = `
    SELECT test, count(*)
        FROM examenes e
            left join asignaciones a using (test)
            left join trimestres t using (trimestre)
        GROUP BY count(*)
        HAVING count(*) > 1
    `;

asdasda
asdasdaasd
asd