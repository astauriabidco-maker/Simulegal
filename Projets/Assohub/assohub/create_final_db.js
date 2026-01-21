const { Pool } = require('pg');
const pool = new Pool({ connectionString: 'postgresql://postgres:postgres@localhost:51214/template1?sslmode=disable' });

async function createDb() {
    try {
        console.log('Creating database assohub_final...');
        // We can't use DROP DATABASE in a transaction, but pool.query usually runs in its own if not specified.
        // However, to be safe, we use a client.
        const client = await pool.connect();
        try {
            await client.query('DROP DATABASE IF EXISTS assohub_final');
            await client.query('CREATE DATABASE assohub_final');
            console.log('Database assohub_final created successfully.');
        } finally {
            client.release();
        }
    } catch (err) {
        console.error('Error creating database:', err);
    } finally {
        await pool.end();
    }
}

createDb();
