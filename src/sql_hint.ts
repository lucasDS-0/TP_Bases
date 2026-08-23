

// sql-hint name empleados
var sqlExcesoAsignaciones = `
    SELECT empleado, count(*)
        FROM empleados
    `;

asd
asdasd
asd



// sql-hint join( join(alias e, alias a, allow-no-pk), alias t, allow-no-pk)
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