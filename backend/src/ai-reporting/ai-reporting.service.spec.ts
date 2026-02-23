import { Test, TestingModule } from '@nestjs/testing';
import { AiReportingService } from './ai-reporting.service';

describe('AiReportingService', () => {
  let service: AiReportingService;

  beforeEach(async () => {
    const module: TestingModule = await Test.createTestingModule({
      providers: [AiReportingService],
    }).compile();

    service = module.get<AiReportingService>(AiReportingService);
  });

  it('should be defined', () => {
    expect(service).toBeDefined();
  });
});
