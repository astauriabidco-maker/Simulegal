import { Test, TestingModule } from '@nestjs/testing';
import { AiReportingController } from './ai-reporting.controller';

describe('AiReportingController', () => {
  let controller: AiReportingController;

  beforeEach(async () => {
    const module: TestingModule = await Test.createTestingModule({
      controllers: [AiReportingController],
    }).compile();

    controller = module.get<AiReportingController>(AiReportingController);
  });

  it('should be defined', () => {
    expect(controller).toBeDefined();
  });
});
