import { NestFactory } from '@nestjs/core';
import { AppModule } from './app.module';

async function bootstrap() {
  const app = await NestFactory.create(AppModule, { rawBody: true });
  app.enableCors(); // Autorise les requêtes cross-origin
  await app.listen(process.env.PORT || 5000);
}
bootstrap();
