import { CatalogService } from './catalog.service';
export declare class CatalogController {
    private readonly service;
    constructor(service: CatalogService);
    getServices(): {
        id: string;
        name: string;
        description: string;
        category: string;
    }[];
}
