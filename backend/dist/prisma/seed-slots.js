"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const client_1 = require("@prisma/client");
const prisma = new client_1.PrismaClient();
async function main() {
    console.log('🌱 Seeding appointment slots...');
    let jurist = await prisma.user.findFirst({ where: { role: 'HQ_ADMIN' } });
    if (!jurist) {
        jurist = await prisma.user.findFirst();
    }
    if (!jurist) {
        console.log('❌ No user found to assign slots. Please create a user first.');
        return;
    }
    console.log(`👤 Using user ${jurist.email} (${jurist.id}) as Jurist`);
    const today = new Date();
    const slots = [];
    for (let i = 0; i < 14; i++) {
        const day = new Date(today);
        day.setDate(day.getDate() + i);
        if (day.getDay() === 0 || day.getDay() === 6)
            continue;
        const hours = [10, 11, 14, 15, 16];
        for (const h of hours) {
            const start = new Date(day);
            start.setHours(h, 0, 0, 0);
            const end = new Date(start);
            end.setHours(h + 1, 0, 0, 0);
            slots.push({
                juristId: jurist.id,
                start,
                end,
                status: 'AVAILABLE'
            });
        }
    }
    await prisma.appointmentSlot.deleteMany({ where: { status: 'AVAILABLE' } });
    for (const slot of slots) {
        await prisma.appointmentSlot.create({ data: slot });
    }
    console.log(`✅ Created ${slots.length} APPOINTMENT SLOTS.`);
}
main()
    .catch(e => console.error(e))
    .finally(async () => {
    await prisma.$disconnect();
});
//# sourceMappingURL=seed-slots.js.map