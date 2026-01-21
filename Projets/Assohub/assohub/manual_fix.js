const { Pool } = require('pg');
const pool = new Pool({ connectionString: 'postgresql://postgres:postgres@localhost:51214/template1?sslmode=disable' });

async function fix() {
    try {
        console.log('Creating Association table...');
        await pool.query(`
      CREATE TABLE IF NOT EXISTS "Association" (
        "id" TEXT PRIMARY KEY,
        "name" TEXT NOT NULL,
        "settings" JSONB NOT NULL,
        "subscription_plan" TEXT NOT NULL DEFAULT 'FREE',
        "is_active" BOOLEAN NOT NULL DEFAULT true,
        "createdAt" TIMESTAMP(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
        "updatedAt" TIMESTAMP(3) NOT NULL
      )
    `);

        console.log('Creating User table...');
        await pool.query(`
      CREATE TABLE IF NOT EXISTS "User" (
        "id" TEXT PRIMARY KEY,
        "associationId" TEXT NOT NULL,
        "email" TEXT NOT NULL,
        "password_hash" TEXT NOT NULL,
        "firstName" TEXT,
        "lastName" TEXT,
        "phone" TEXT,
        "role" TEXT NOT NULL DEFAULT 'MEMBER',
        "status" TEXT NOT NULL DEFAULT 'ACTIVE',
        "createdAt" TIMESTAMP(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
        "updatedAt" TIMESTAMP(3) NOT NULL,
        CONSTRAINT "User_associationId_fkey" FOREIGN KEY ("associationId") REFERENCES "Association"("id") ON DELETE RESTRICT ON UPDATE CASCADE
      )
    `);
        console.log('Tables created successfully.');
    } catch (err) {
        console.error('Error creating tables:', err);
    } finally {
        await pool.end();
    }
}

fix();
