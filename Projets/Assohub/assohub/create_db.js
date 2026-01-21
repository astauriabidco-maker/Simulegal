const { Client } = require('pg');
const client = new Client({ connectionString: 'postgresql://postgres:postgres@localhost:51214/postgres?sslmode=disable' });

async function createDb() {
    try {
        await client.connect();
        // We need to use 127.0.0.1 if localhost fails
        console.log('Connected to postgres db. Attempting to create assohub_v2...');
        await client.query('DROP DATABASE IF EXISTS assohub_v2');
        await client.query('CREATE DATABASE assohub_v2');
        console.log('Database assohub_v2 created successfully.');
    } catch (err) {
        console.error('Error creating DB:', err);
    } finally {
        await client.end();
    }
}

createDb();
