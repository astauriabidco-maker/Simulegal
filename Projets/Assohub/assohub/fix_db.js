const { Pool } = require('pg');
const pool = new Pool({ connectionString: 'postgresql://postgres:postgres@localhost:51214/postgres?sslmode=disable' });

async function fix() {
    try {
        console.log('Attempting to add associationId column to User table...');
        await pool.query('ALTER TABLE "User" ADD COLUMN IF NOT EXISTS "associationId" TEXT');
        console.log('Column added or already exists.');

        // Add foreign key if not exists (might fail if constraint already exists, choosing simple for now)
        try {
            await pool.query('ALTER TABLE "User" ADD CONSTRAINT "User_associationId_fkey" FOREIGN KEY ("associationId") REFERENCES "Association"("id") ON DELETE RESTRICT ON UPDATE CASCADE');
            console.log('Foreign key constraint added.');
        } catch (e) {
            console.log('Could not add constraint (might already exist):', e.message);
        }
    } catch (err) {
        console.error('Error fixing DB:', err);
    } finally {
        await pool.end();
    }
}

fix();
