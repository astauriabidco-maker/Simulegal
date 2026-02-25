-- RedefineTables
PRAGMA defer_foreign_keys=ON;
PRAGMA foreign_keys=OFF;
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
    "lastCallOutcome" TEXT
);
INSERT INTO "new_Prospect" ("address", "agencyId", "assignedToSalesId", "campaignName", "city", "convertedLeadId", "country", "createdAt", "email", "firstName", "id", "interestServiceId", "lastContactAt", "lastName", "phone", "score", "source", "status", "updatedAt", "zipCode") SELECT "address", "agencyId", "assignedToSalesId", "campaignName", "city", "convertedLeadId", "country", "createdAt", "email", "firstName", "id", "interestServiceId", "lastContactAt", "lastName", "phone", "score", "source", "status", "updatedAt", "zipCode" FROM "Prospect";
DROP TABLE "Prospect";
ALTER TABLE "new_Prospect" RENAME TO "Prospect";
PRAGMA foreign_keys=ON;
PRAGMA defer_foreign_keys=OFF;
