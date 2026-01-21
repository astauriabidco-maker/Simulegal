import * as dotenv from 'dotenv';
import path from 'path';

// Use absolute path to .env file
dotenv.config({ path: '/Users/franklintchakounteu/.gemini/antigravity/scratch/Assohub/assohub/apps/backend/.env' });

import { Logger } from '@nestjs/common';
import { NestFactory } from '@nestjs/core';
import { AppModule } from './app/app.module';

async function bootstrap() {
  const app = await NestFactory.create(AppModule);
  const globalPrefix = 'api';
  app.setGlobalPrefix(globalPrefix);
  app.enableCors();
  const port = process.env.PORT || 3000;
  await app.listen(port);
  Logger.log(
    `🚀 Application is running on: http://localhost:${port}/${globalPrefix}`,
  );
}

bootstrap();
