-- AlterTable
ALTER TABLE "Prospect" ADD COLUMN "address" TEXT;
ALTER TABLE "Prospect" ADD COLUMN "city" TEXT;
ALTER TABLE "Prospect" ADD COLUMN "country" TEXT DEFAULT 'France';
ALTER TABLE "Prospect" ADD COLUMN "zipCode" TEXT;
