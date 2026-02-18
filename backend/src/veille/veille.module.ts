import { Module } from '@nestjs/common';
import { VeilleController } from './veille.controller';
import { VeilleService } from './veille.service';
import { PrismaModule } from '../prisma/prisma.module';

@Module({
    imports: [PrismaModule],
    controllers: [VeilleController],
    providers: [VeilleService],
    exports: [VeilleService],
})
export class VeilleModule { }
