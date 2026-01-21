import { PrismaClient } from '@prisma/client';
import { PrismaBetterSqlite3 } from '@prisma/adapter-better-sqlite3';

async function verify() {
    const dbPath = '/Users/franklintchakounteu/.gemini/antigravity/scratch/Assohub/assohub/apps/backend/prisma/dev.db';
    const adapter = new PrismaBetterSqlite3({ url: 'file:' + dbPath });
    const prisma = new PrismaClient({ adapter } as any);

    const associationId = '2e1e4eb3-62bb-46c0-82f1-53ae7696e9fc';
    const testEmail = `test-${Date.now()}@example.com`;

    try {
        console.log(`Attempting to create user: ${testEmail}`);
        const newUser = await prisma.user.create({
            data: {
                email: testEmail,
                password_hash: 'hashed_password',
                firstName: 'Test',
                lastName: 'User',
                role: 'MEMBER',
                status: 'ACTIVE',
                associationId: associationId,
            },
            select: {
                id: true,
                email: true,
                associationId: true,
            }
        });

        console.log('User created successfully:', newUser);

        // Clean up
        await prisma.user.delete({ where: { id: newUser.id } });
        console.log('Cleanup: Test user deleted.');

    } catch (error) {
        console.error('Verification failed:', error);
        process.exit(1);
    } finally {
        await prisma.$disconnect();
    }
}

verify();
