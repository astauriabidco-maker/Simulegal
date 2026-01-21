const { Pool } = require('pg');
const pool = new Pool({ connectionString: 'postgresql://postgres:postgres@127.0.0.1:51214/template1?sslmode=disable' });

async function reset() {
    try {
        console.log('Nuclear reset of template1...');
        const tables = await pool.query("SELECT tablename FROM pg_tables WHERE schemaname = 'public'");
        for (const row of tables.rows) {
            console.log(`Dropping table ${row.tablename}...`);
            await pool.query(`DROP TABLE IF EXISTS "${row.tablename}" CASCADE`);
        }
        console.log('All tables dropped.');
    } catch (err) {
        console.error('Error during reset:', err);
    } finally {
        await pool.end();
    }
}

reset();
