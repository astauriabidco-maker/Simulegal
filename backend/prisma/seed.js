const { PrismaClient } = require('@prisma/client');

async function main() {
    const prisma = new PrismaClient({
        datasources: {
            db: {
                url: 'file:./dev.db'
            }
        }
    });
    console.log('PrismaClient initialized');

    try {
        const agencies = await prisma.agency.findMany();
        console.log(`Found ${agencies.length} agencies`);
    } catch (e) {
        console.error('Error during query:', e);
    } finally {
        await prisma.$disconnect();
    }
}

main();
