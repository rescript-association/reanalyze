type structural = {x: int, y: int, ...};

// {x: int, y: int, ...}
let s = {x:3, y:4};

// {x: int, y: int, ...} => int
let area = r => r.x + r.y;

// Now x and y become part of the scope
type nominal = {x: int, y: int};

// nominal
let n = {x:3, y:4};

// nominal => int
let area2 = r => r.x + r.y;

// OK: auto subtyping from n:nominal to n:structural
let z = area(n);

// extension: nominal with 3 fields
type nominal3 = {...nominal, z: int}

// extension: structural with 3 fields
type structural3 = {...structural, z: int}

// turn nominal to structural
type nominalToStructural = {...nominal, ...}