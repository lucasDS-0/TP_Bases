
Etiam sit amet orci eget eros faucibus tincidunt. 
Nulla consequat massa quis enim. Nullam quis ante. 

Nullam quis ante. 
Etiam sit amet orci eget eros faucibus tincidunt. Donec sodales sagittis magna. 

// sql-hint 
var sqlExcesoAsignaciones = `
    SELECT empleado, count(*)
        FROM empleados
    `;

Etiam ultricies nisi vel augue. Vivamus elementum semper nisi. 
Vivamus elementum semper nisi. Aenean commodo ligula eget dolor. 
Donec sodales sagittis magna. Curabitur ullamcorper ultricies nisi. 
Maecenas tempus, tellus eget condimentum rhoncus, 

// sql-hint name
var sqlExcesoAsignaciones = `
    SELECT empleado, count(*)
        FROM empleados
    `;

Donec sodales sagittis magna. Phasellus viverra nulla ut metus varius laoreet. 
Aenean commodo ligula eget dolor. 
Sed fringilla mauris sit amet nibh. Nam eget dui. 
Donec sodales sagittis magna. Nullam quis ante. 


// sql-hint name asignaciones
var sqlExcesoAsignaciones = `
    SELECT empleado, count(*)
        FROM empleados
    `;

Donec sodales sagittis magna. Phasellus viverra nulla ut metus varius laoreet. 
Aenean commodo ligula eget dolor. 
Sed fringilla mauris sit amet nibh. Nam eget dui. 
Donec sodales sagittis magna. Nullam quis ante. 


// sql-hint alias empleados
var sqlExcesoAsignaciones = `
    SELECT empleado, count(*)
        FROM empleados
    `;

Nam quam nunc, blandit vel, luctus pulvinar, hendrerit id, lorem. 
Nulla consequat massa quis enim. 

Phasellus viverra nulla ut metus varius laoreet. 
Phasellus viverra nulla ut metus varius laoreet. 

// sql-hint name empleados
var sqlExcesoAsignaciones = `
    SELECT empleado, count(*)
        FROM empleados e
    `;

Nam quam nunc, blandit vel, luctus pulvinar, hendrerit id, lorem. 
Nulla consequat massa quis enim. 

Phasellus viverra nulla ut metus varius laoreet. 
Phasellus viverra nulla ut metus varius laoreet. 

// sql-hint name empleados
var sqlExcesoAsignaciones = `
    SELECT empleado, count(*)
        FROM empleados
    `;

Donec sodales sagittis magna. Donec sodales sagittis magna. 

// sql-hint join( join(alias e, alias a, no-flag), alias t, no-flag)
var sqlExcesoAsignaciones = `
    SELECT empleado, count(*)
        FROM empleados e
            left join asignaciones a using (empleado)
            left join trimestres t using (trimestre)
        GROUP BY count(*)
        HAVING count(*) > 1
    `;

Maecenas tempus, tellus eget condimentum rhoncus, sem 
Donec pede justo, fringilla vel, aliquet nec, vulputate eget, arcu.

// sql-hint join( join(alias e, alias a, no-flag), alias t, no-flag)
var sqlExcesoAsignaciones = `
    SELECT empleado, count(*)
        FROM empleados e
            left join asignaciones a using (empleado)
            left join trimestres t using (desc)
        GROUP BY count(*)
        HAVING count(*) > 1
    `;

Maecenas tempus, tellus eget condimentum rhoncus, sem 
Donec pede justo, fringilla vel, aliquet nec, vulputate eget, arcu.


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

sit amet adipiscing sem neque sed ipsum. In enim justo, rhoncus ut

// sql-hint join(join(alias e, alias a, allow-no-pk), alias t, allow-no-pk)
var sqlExcesoAsignaciones = `
    SELECT test, count(*)
        FROM examenes e
            left join asignaciones a using (test)
            left join trimestres t using (trimestre)
        GROUP BY count(*)
        HAVING count(*) > 1
    `;

sit amet adipiscing sem neque sed ipsum.