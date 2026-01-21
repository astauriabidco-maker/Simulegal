const { Pool } = require('pg');
const pool = new Pool({ connectionString: 'postgresql://postgres:postgres@localhost:51214/template1?sslmode=disable' });

async function check() {
    try {
        const res = await pool.query("SELECT column_name FROM information_schema.columns WHERE table_name = 'User'");
        console.log('Columns in User table:', res.rows.map(r => r.column_name));

        const tables = await pool.query("SELECT table_name FROM information_schema.tables WHERE table_schema = 'public'");
        console.log('Tables in database:', tables.rows.map(r => r.table_name));
    } catch (err) {
        console.error('Error checking DB:', err);
    } finally {
        await pool.end();
    }
}

check();
