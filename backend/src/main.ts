import { NestFactory } from '@nestjs/core';
import { AppModule } from './app.module';

async function bootstrap() {
  const app = await NestFactory.create(AppModule);
  app.enableCors(); // Autorise les requêtes cross-origin
  await app.listen(3001);
}
bootstrap();
