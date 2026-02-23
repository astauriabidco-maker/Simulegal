import { Module, forwardRef } from '@nestjs/common';
import { CallLogsController } from './call-logs.controller';
import { CallLogsService } from './call-logs.service';
import { PrismaService } from '../../prisma/prisma.service';
import { SalesModule } from '../sales.module';

@Module({
    imports: [forwardRef(() => SalesModule)],
    controllers: [CallLogsController],
    providers: [CallLogsService, PrismaService],
    exports: [CallLogsService],
})
export class CallLogsModule { }
