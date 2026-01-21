import { Module } from '@nestjs/common';
import { CallLogsController } from './call-logs.controller';
import { CallLogsService } from './call-logs.service';
import { PrismaService } from '../../prisma/prisma.service';

@Module({
    controllers: [CallLogsController],
    providers: [CallLogsService, PrismaService],
    exports: [CallLogsService],
})
export class CallLogsModule { }
