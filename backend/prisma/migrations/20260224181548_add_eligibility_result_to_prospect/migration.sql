-- CreateTable
CREATE TABLE "SalesAppointment" (
    "id" TEXT NOT NULL PRIMARY KEY,
    "prospectId" TEXT NOT NULL,
    "date" DATETIME NOT NULL,
    "agencyId" TEXT NOT NULL,
    "agencyName" TEXT NOT NULL,
    "serviceId" TEXT,
    "status" TEXT NOT NULL DEFAULT 'SCHEDULED',
    "meetingLink" TEXT,
    "confirmationSent" BOOLEAN NOT NULL DEFAULT false,
    "confirmationSentVia" TEXT,
    "notes" TEXT,
    "createdAt" DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updatedAt" DATETIME NOT NULL,
    CONSTRAINT "SalesAppointment_prospectId_fkey" FOREIGN KEY ("prospectId") REFERENCES "Prospect" ("id") ON DELETE RESTRICT ON UPDATE CASCADE
);

-- RedefineTables
PRAGMA defer_foreign_keys=ON;
PRAGMA foreign_keys=OFF;
CREATE TABLE "new_Appointment" (
    "id" TEXT NOT NULL PRIMARY KEY,
    "start" DATETIME NOT NULL,
    "end" DATETIME NOT NULL,
    "type" TEXT NOT NULL,
    "status" TEXT NOT NULL DEFAULT 'SCHEDULED',
    "leadId" TEXT,
    "leadName" TEXT NOT NULL,
    "leadEmail" TEXT,
    "prospectId" TEXT,
    "agencyId" TEXT,
    "hostUserId" TEXT,
    "meetingLink" TEXT,
    "serviceId" TEXT,
    "cancellationReason" TEXT,
    "createdAt" DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updatedAt" DATETIME NOT NULL,
    CONSTRAINT "Appointment_agencyId_fkey" FOREIGN KEY ("agencyId") REFERENCES "Agency" ("id") ON DELETE SET NULL ON UPDATE CASCADE,
    CONSTRAINT "Appointment_hostUserId_fkey" FOREIGN KEY ("hostUserId") REFERENCES "User" ("id") ON DELETE SET NULL ON UPDATE CASCADE,
    CONSTRAINT "Appointment_leadId_fkey" FOREIGN KEY ("leadId") REFERENCES "Lead" ("id") ON DELETE SET NULL ON UPDATE CASCADE,
    CONSTRAINT "Appointment_prospectId_fkey" FOREIGN KEY ("prospectId") REFERENCES "Prospect" ("id") ON DELETE SET NULL ON UPDATE CASCADE
);
INSERT INTO "new_Appointment" ("agencyId", "cancellationReason", "createdAt", "end", "hostUserId", "id", "leadEmail", "leadId", "leadName", "meetingLink", "serviceId", "start", "status", "type", "updatedAt") SELECT "agencyId", "cancellationReason", "createdAt", "end", "hostUserId", "id", "leadEmail", "leadId", "leadName", "meetingLink", "serviceId", "start", "status", "type", "updatedAt" FROM "Appointment";
DROP TABLE "Appointment";
ALTER TABLE "new_Appointment" RENAME TO "Appointment";
CREATE TABLE "new_Prospect" (
    "id" TEXT NOT NULL PRIMARY KEY,
    "firstName" TEXT NOT NULL,
    "lastName" TEXT NOT NULL,
    "phone" TEXT NOT NULL,
    "email" TEXT,
    "address" TEXT,
    "city" TEXT,
    "zipCode" TEXT,
    "country" TEXT DEFAULT 'France',
    "source" TEXT NOT NULL DEFAULT 'MANUAL',
    "campaignName" TEXT,
    "interestServiceId" TEXT,
    "score" INTEGER NOT NULL DEFAULT 0,
    "agencyId" TEXT NOT NULL,
    "assignedToSalesId" TEXT,
    "status" TEXT NOT NULL DEFAULT 'NEW',
    "convertedLeadId" TEXT,
    "createdAt" DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updatedAt" DATETIME NOT NULL,
    "lastContactAt" DATETIME,
    "callAttempts" INTEGER NOT NULL DEFAULT 0,
    "noAnswerCount" INTEGER NOT NULL DEFAULT 0,
    "callbackCount" INTEGER NOT NULL DEFAULT 0,
    "callbackRequestedAt" DATETIME,
    "callbackScheduledAt" DATETIME,
    "lastCallOutcome" TEXT,
    "noShowCount" INTEGER NOT NULL DEFAULT 0,
    "qualifiedAt" DATETIME,
    "stageEnteredAt" DATETIME DEFAULT CURRENT_TIMESTAMP,
    "lostReason" TEXT,
    "appointment" JSONB,
    "eligibilityResult" JSONB
);
INSERT INTO "new_Prospect" ("address", "agencyId", "assignedToSalesId", "callAttempts", "callbackCount", "callbackRequestedAt", "campaignName", "city", "convertedLeadId", "country", "createdAt", "email", "firstName", "id", "interestServiceId", "lastCallOutcome", "lastContactAt", "lastName", "noAnswerCount", "phone", "score", "source", "status", "updatedAt", "zipCode") SELECT "address", "agencyId", "assignedToSalesId", "callAttempts", "callbackCount", "callbackRequestedAt", "campaignName", "city", "convertedLeadId", "country", "createdAt", "email", "firstName", "id", "interestServiceId", "lastCallOutcome", "lastContactAt", "lastName", "noAnswerCount", "phone", "score", "source", "status", "updatedAt", "zipCode" FROM "Prospect";
DROP TABLE "Prospect";
ALTER TABLE "new_Prospect" RENAME TO "Prospect";
PRAGMA foreign_keys=ON;
PRAGMA defer_foreign_keys=OFF;
