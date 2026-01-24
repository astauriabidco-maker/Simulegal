"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const client_1 = require("@prisma/client");
const prisma = new client_1.PrismaClient();
async function main() {
    const agencies = await prisma.agency.findMany();
    console.log(JSON.stringify(agencies, null, 2));
}
main().catch(e => console.error(e)).finally(() => prisma.$disconnect());
//# sourceMappingURL=list-agencies.js.map