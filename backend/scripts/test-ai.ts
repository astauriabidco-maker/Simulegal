import { NestFactory } from '@nestjs/core';
import { AppModule } from '../src/app.module';
import { SupervisionAgentService } from '../src/agents/supervision-agent.service';
import { PrismaService } from '../src/prisma/prisma.service';
import { EventEmitter2 } from '@nestjs/event-emitter';

async function bootstrap() {
    console.log('⏳ Bootstrapping Nest Application Context (This might take a few seconds)...');
    const app = await NestFactory.createApplicationContext(AppModule, { logger: ['error', 'warn'] });
    const supervision = app.get(SupervisionAgentService);
    const prisma = app.get(PrismaService);
    const events = app.get(EventEmitter2);

    console.log('🔄 Préparation des données de test...');

    const randomSuffix = Math.floor(Math.random() * 10000).toString();

    const commonLeadData = {
        serviceId: 'test-service',
        serviceName: 'Création SASU',
    };

    // 1. Fraude Paiement
    const fraudLead = await prisma.lead.create({
        data: {
            id: `test-fraud-${randomSuffix}`,
            name: 'Jean Dubois',
            email: `jean.dubois+${randomSuffix}@test.com`,
            phone: '33612345678',
            ...commonLeadData
        }
    });

    // 2. OCR Inconsistency
    const ocrLead = await prisma.lead.create({
        data: {
            id: `test-ocr-${randomSuffix}`,
            name: 'Robert Dupont',
            email: `robert.dupont+${randomSuffix}@test.com`,
            phone: '33512344321',
            ...commonLeadData,
            documents: JSON.stringify([
                { status: 'VALID', ocrData: { lastName: 'Dupont', birthDate: '01/01/1980' } },
                { status: 'VALID', ocrData: { lastName: 'Dupin', birthDate: '01/01/1980' } }
            ])
        }
    });

    // 3. NLP WhatsApp
    const nlpLead = await prisma.lead.create({
        data: {
            id: `test-nlp-${randomSuffix}`,
            name: 'Alice Martin',
            email: `alice.martin+${randomSuffix}@test.com`,
            phone: '33687654321',
            ...commonLeadData,
        }
    });

    // 4. CRON - Unpaid
    let d2 = new Date();
    d2.setDate(d2.getDate() - 3);
    const unpaidLead = await prisma.lead.create({
        data: {
            id: `test-unpaid-${randomSuffix}`,
            name: 'Marc Unpaid',
            email: `unpaid+${randomSuffix}@test.com`,
            phone: '33700000000',
            ...commonLeadData,
            createdAt: d2,
            amountPaid: 0
        }
    });

    // 5. CRON - Stuck
    let d7 = new Date();
    d7.setDate(d7.getDate() - 8);
    const stuckLead = await prisma.lead.create({
        data: {
            id: `test-stuck-${randomSuffix}`,
            name: 'Paul Stuck',
            email: `stuck+${randomSuffix}@test.com`,
            phone: '33699999999',
            ...commonLeadData,
            status: 'COLLECTING',
            stageEnteredAt: d7,
            requiredDocs: JSON.stringify(['CNI', 'Justificatif']),
            documents: JSON.stringify([])
        }
    });

    // 6. CRON - Dormant
    let d180 = new Date();
    d180.setMonth(d180.getMonth() - 7);
    const dormantLead = await prisma.lead.create({
        data: {
            id: `test-dormant-${randomSuffix}`,
            name: 'Sophie Dormant',
            email: `dormant+${randomSuffix}@test.com`,
            phone: '33688888888',
            ...commonLeadData,
            status: 'NEW',
            stageEnteredAt: d180
        }
    });

    console.log('✅ Leads de test créés. Déclenchement des événements...\n');

    console.log('--- 🚀 TEST 1: Fraud Check ---');
    console.log(`>> Titulaire du dossier: ${fraudLead.name} | Payeur Stripe: Vladimir Ivanov`);
    events.emit('lead.payment.received', {
        leadId: fraudLead.id,
        amount: 50000,
        sessionId: 'test_session_' + randomSuffix,
        customerEmail: 'hacker@darkweb.ru',
        customerName: 'Vladimir Ivanov'
    });

    console.log('\n--- 🚀 TEST 2: OCR Cross-Check ---');
    console.log(`>> Noms trouvés dans les PDFs: Dupont vs Dupin`);
    events.emit('lead.document.validated', { leadId: ocrLead.id });

    console.log('\n--- 🚀 TEST 3: NLP WhatsApp ---');
    const waMsg = "Aidez moi on me dit que mon dossier de régularisation est bloqué à la prefecture, je risque l'expulsion !!!";
    console.log(`>> Message WhatsApp reçu: "${waMsg}"`);
    events.emit('whatsapp.message.received', {
        leadId: nlpLead.id,
        message: waMsg,
        senderName: nlpLead.name,
        senderPhone: nlpLead.phone
    });

    console.log('\n⏳ Attente (Ollama LLM peut prendre quelques secondes pour parser le texte)...');
    await new Promise(r => setTimeout(r, 12000));

    console.log('\n--- 🚀 TEST 4: Tâche CRON de Nuit (Unpaid, Stuck, Dormant) ---');
    console.log('>> Lancement manuel de la tâche de Minuit...');
    await supervision.checkNightlyStuckAndUnpaidLeads();

    // Verification
    console.log('\n======================================');
    console.log('📋 RÉSULTATS DES WHISPER BOX GÉNÉRÉES:');
    console.log('======================================');

    const notes = await prisma.leadNote.findMany({
        where: {
            leadId: { in: [fraudLead.id, ocrLead.id, nlpLead.id, unpaidLead.id, stuckLead.id, dormantLead.id] }
        }
    });

    for (const note of notes) {
        console.log(`\n--- Note pour le dossier: ${note.leadId} ---`);
        console.log(`🗣️  Auteur: ${note.author}`);
        console.log(`📝 Contenu:\n${note.content}`);
    }

    // Cleanup
    console.log('\n\n🧹 Nettoyage des données de test...');
    await prisma.leadNote.deleteMany({ where: { leadId: { in: [fraudLead.id, ocrLead.id, nlpLead.id, unpaidLead.id, stuckLead.id, dormantLead.id] } } });
    await prisma.lead.deleteMany({ where: { id: { in: [fraudLead.id, ocrLead.id, nlpLead.id, unpaidLead.id, stuckLead.id, dormantLead.id] } } });

    console.log('✅ Démonstration terminée.');
    await app.close();
    process.exit(0);
}

bootstrap().catch(err => {
    console.error(err);
    process.exit(1);
});
