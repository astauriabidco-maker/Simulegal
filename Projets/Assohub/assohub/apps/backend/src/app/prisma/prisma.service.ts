import { Injectable, OnModuleInit, OnModuleDestroy } from '@nestjs/common';
import { PrismaClient } from '@prisma/client';
import { PrismaBetterSqlite3 } from '@prisma/adapter-better-sqlite3';
import Database from 'better-sqlite3';

@Injectable()
export class PrismaService extends PrismaClient implements OnModuleInit, OnModuleDestroy {
  constructor() {
    const dbPath = '/Users/franklintchakounteu/.gemini/antigravity/scratch/Assohub/assohub/apps/backend/prisma/dev.db';
    // PrismaBetterSqlite3 expects a config object with 'url', not a Database instance
    const adapter = new PrismaBetterSqlite3({ url: 'file:' + dbPath });
    super({ adapter } as any);
  }

  async onModuleInit() {
    console.log('PrismaService: Connecting to SQLite database...');
    await this.$connect();
    console.log('PrismaService: Successfully connected to SQLite.');
  }

  async onModuleDestroy() {
    await this.$disconnect();
  }
}
