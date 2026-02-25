const fs = require('fs');
let file = fs.readFileSync('/Users/user/Documents/DEVELOPPEMENTS/PROJETS/SIMULEGAL/Simulegal/types/index.ts', 'utf8');
file = file.replace(/:\s*\{([^}]*)\}/g, (match, body) => {
    let newBody = body.replace(/(\w+)(:\s*[a-zA-Z_'|0-9 ]+;)/g, '$1?$2');
    return `: {${newBody}}`;
});
fs.writeFileSync('/Users/user/Documents/DEVELOPPEMENTS/PROJETS/SIMULEGAL/Simulegal/types/index.ts', file);
