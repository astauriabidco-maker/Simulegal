import { evaluateRule, RuleCondition } from './rule-engine.util';

describe('RuleEngine — evaluateRule', () => {
    // ─── Basic Operators ──────────────────────────────────────────

    describe('EQ operator', () => {
        it('returns true when values are equal', () => {
            const data = { identity: { age: 25 } };
            const condition: RuleCondition = { var: 'identity.age', op: 'EQ', val: 25 };
            expect(evaluateRule(data, condition)).toBe(true);
        });

        it('returns false when values differ', () => {
            const data = { identity: { age: 30 } };
            const condition: RuleCondition = { var: 'identity.age', op: 'EQ', val: 25 };
            expect(evaluateRule(data, condition)).toBe(false);
        });

        it('handles boolean values', () => {
            const data = { civic: { clean_criminal_record: true } };
            expect(evaluateRule(data, { var: 'civic.clean_criminal_record', op: 'EQ', val: true })).toBe(true);
            expect(evaluateRule(data, { var: 'civic.clean_criminal_record', op: 'EQ', val: false })).toBe(false);
        });

        it('handles string values', () => {
            const data = { identity: { nationality_group: 'REFUGEE' } };
            expect(evaluateRule(data, { var: 'identity.nationality_group', op: 'EQ', val: 'REFUGEE' })).toBe(true);
            expect(evaluateRule(data, { var: 'identity.nationality_group', op: 'EQ', val: 'EU' })).toBe(false);
        });
    });

    describe('NEQ operator', () => {
        it('returns true when values differ', () => {
            const data = { identity: { nationality_group: 'NON_EU' } };
            expect(evaluateRule(data, { var: 'identity.nationality_group', op: 'NEQ', val: 'EU' })).toBe(true);
        });

        it('returns false when values equal', () => {
            const data = { identity: { nationality_group: 'EU' } };
            expect(evaluateRule(data, { var: 'identity.nationality_group', op: 'NEQ', val: 'EU' })).toBe(false);
        });
    });

    describe('GT / GTE / LT / LTE operators', () => {
        const data = { identity: { age: 25 }, timeline: { years_continuous_residence: 5 } };

        it('GT: strictly greater than', () => {
            expect(evaluateRule(data, { var: 'identity.age', op: 'GT', val: 24 })).toBe(true);
            expect(evaluateRule(data, { var: 'identity.age', op: 'GT', val: 25 })).toBe(false);
        });

        it('GTE: greater than or equal', () => {
            expect(evaluateRule(data, { var: 'identity.age', op: 'GTE', val: 25 })).toBe(true);
            expect(evaluateRule(data, { var: 'identity.age', op: 'GTE', val: 26 })).toBe(false);
        });

        it('LT: strictly less than', () => {
            expect(evaluateRule(data, { var: 'identity.age', op: 'LT', val: 26 })).toBe(true);
            expect(evaluateRule(data, { var: 'identity.age', op: 'LT', val: 25 })).toBe(false);
        });

        it('LTE: less than or equal', () => {
            expect(evaluateRule(data, { var: 'identity.age', op: 'LTE', val: 25 })).toBe(true);
            expect(evaluateRule(data, { var: 'identity.age', op: 'LTE', val: 24 })).toBe(false);
        });
    });

    describe('IN operator', () => {
        it('returns true when value is in the list', () => {
            const data = { identity: { nationality_group: 'REFUGEE' } };
            expect(evaluateRule(data, { var: 'identity.nationality_group', op: 'IN', val: ['REFUGEE', 'STATELESS'] })).toBe(true);
        });

        it('returns false when value is not in the list', () => {
            const data = { identity: { nationality_group: 'EU' } };
            expect(evaluateRule(data, { var: 'identity.nationality_group', op: 'IN', val: ['REFUGEE', 'STATELESS'] })).toBe(false);
        });
    });

    // ─── Missing/null values ──────────────────────────────────────

    describe('missing or null values', () => {
        it('returns false when variable path does not exist', () => {
            const data = { identity: { age: 25 } };
            expect(evaluateRule(data, { var: 'work.contract_type', op: 'EQ', val: 'CDI' })).toBe(false);
        });

        it('returns false when value is null', () => {
            const data = { identity: { age: null } };
            expect(evaluateRule(data, { var: 'identity.age', op: 'GTE', val: 18 })).toBe(false);
        });

        it('returns false when value is undefined', () => {
            const data = { identity: {} };
            expect(evaluateRule(data, { var: 'identity.age', op: 'GTE', val: 18 })).toBe(false);
        });
    });

    // ─── AND / OR groups ──────────────────────────────────────────

    describe('AND conditions', () => {
        it('returns true when ALL subconditions are true', () => {
            const data = { identity: { age: 25 }, civic: { clean_criminal_record: true } };
            const condition: RuleCondition = {
                AND: [
                    { var: 'identity.age', op: 'GTE', val: 18 },
                    { var: 'civic.clean_criminal_record', op: 'EQ', val: true },
                ]
            };
            expect(evaluateRule(data, condition)).toBe(true);
        });

        it('returns false when ONE subcondition is false', () => {
            const data = { identity: { age: 15 }, civic: { clean_criminal_record: true } };
            const condition: RuleCondition = {
                AND: [
                    { var: 'identity.age', op: 'GTE', val: 18 },
                    { var: 'civic.clean_criminal_record', op: 'EQ', val: true },
                ]
            };
            expect(evaluateRule(data, condition)).toBe(false);
        });
    });

    describe('OR conditions', () => {
        it('returns true when AT LEAST ONE subcondition is true', () => {
            const data = { family: { has_french_child: true, spouse_nationality: 'NON_EU' } };
            const condition: RuleCondition = {
                OR: [
                    { var: 'family.has_french_child', op: 'EQ', val: true },
                    { var: 'family.spouse_nationality', op: 'EQ', val: 'FRENCH' },
                ]
            };
            expect(evaluateRule(data, condition)).toBe(true);
        });

        it('returns false when NO subcondition is true', () => {
            const data = { family: { has_french_child: false, spouse_nationality: 'NON_EU' } };
            const condition: RuleCondition = {
                OR: [
                    { var: 'family.has_french_child', op: 'EQ', val: true },
                    { var: 'family.spouse_nationality', op: 'EQ', val: 'FRENCH' },
                ]
            };
            expect(evaluateRule(data, condition)).toBe(false);
        });
    });

    describe('nested AND/OR', () => {
        it('evaluates nested groups correctly', () => {
            const data = {
                identity: { age: 22, nationality_group: 'NON_EU' },
                family: { has_french_child: true },
                civic: { clean_criminal_record: true },
            };
            const condition: RuleCondition = {
                AND: [
                    { var: 'identity.age', op: 'GTE', val: 18 },
                    { var: 'civic.clean_criminal_record', op: 'EQ', val: true },
                    {
                        OR: [
                            { var: 'identity.nationality_group', op: 'EQ', val: 'REFUGEE' },
                            { var: 'family.has_french_child', op: 'EQ', val: true },
                        ]
                    }
                ]
            };
            expect(evaluateRule(data, condition)).toBe(true);
        });

        it('returns false when inner OR fails in nested AND', () => {
            const data = {
                identity: { age: 22, nationality_group: 'NON_EU' },
                family: { has_french_child: false },
                civic: { clean_criminal_record: true },
            };
            const condition: RuleCondition = {
                AND: [
                    { var: 'identity.age', op: 'GTE', val: 18 },
                    {
                        OR: [
                            { var: 'identity.nationality_group', op: 'EQ', val: 'REFUGEE' },
                            { var: 'family.has_french_child', op: 'EQ', val: true },
                        ]
                    }
                ]
            };
            expect(evaluateRule(data, condition)).toBe(false);
        });
    });

    // ─── French Level (CECRL) semantic comparison ─────────────────

    describe('French level (CECRL) comparison', () => {
        it('GTE: B1 >= A2 is true', () => {
            const data = { integration: { french_level: 'B1' } };
            expect(evaluateRule(data, { var: 'integration.french_level', op: 'GTE', val: 'A2' })).toBe(true);
        });

        it('GTE: A2 >= B1 is false', () => {
            const data = { integration: { french_level: 'A2' } };
            expect(evaluateRule(data, { var: 'integration.french_level', op: 'GTE', val: 'B1' })).toBe(false);
        });

        it('GTE: B1 >= B1 is true (equal)', () => {
            const data = { integration: { french_level: 'B1' } };
            expect(evaluateRule(data, { var: 'integration.french_level', op: 'GTE', val: 'B1' })).toBe(true);
        });

        it('LT: A1 < B2 is true', () => {
            const data = { integration: { french_level: 'A1' } };
            expect(evaluateRule(data, { var: 'integration.french_level', op: 'LT', val: 'B2' })).toBe(true);
        });

        it('GT: C1 > B2 is true', () => {
            const data = { integration: { french_level: 'C1' } };
            expect(evaluateRule(data, { var: 'integration.french_level', op: 'GT', val: 'B2' })).toBe(true);
        });
    });

    // ─── @config: threshold resolution ────────────────────────────

    describe('@config: threshold resolution', () => {
        const thresholds = {
            smic_mensuel_net: 1398,
            salaires: { passeport_talent_annuel: 53836.5 },
        };

        it('resolves @config: reference by dot path', () => {
            const data = { work: { annual_gross_salary: 55000 } };
            const condition: RuleCondition = {
                var: 'work.annual_gross_salary',
                op: 'GTE',
                val: '@config:salaires.passeport_talent_annuel'
            };
            expect(evaluateRule(data, condition, thresholds)).toBe(true);
        });

        it('fails when value below threshold', () => {
            const data = { work: { annual_gross_salary: 40000 } };
            const condition: RuleCondition = {
                var: 'work.annual_gross_salary',
                op: 'GTE',
                val: '@config:salaires.passeport_talent_annuel'
            };
            expect(evaluateRule(data, condition, thresholds)).toBe(false);
        });

        it('resolves top-level @config: path', () => {
            const data = { work: { salary_monthly_gross: 1500 } };
            const condition: RuleCondition = {
                var: 'work.salary_monthly_gross',
                op: 'GTE',
                val: '@config:smic_mensuel_net'
            };
            expect(evaluateRule(data, condition, thresholds)).toBe(true);
        });
    });

    // ─── Real-world rule patterns ─────────────────────────────────

    describe('real-world rule patterns', () => {
        it('matches Carte de Résident – Réfugié (refugee profile)', () => {
            const profile = {
                identity: { age: 35, nationality_group: 'REFUGEE' },
                civic: { clean_criminal_record: true, no_expulsion_order: true },
            };
            const condition: RuleCondition = {
                AND: [
                    { var: 'identity.age', op: 'GTE', val: 18 },
                    { var: 'identity.nationality_group', op: 'EQ', val: 'REFUGEE' },
                    { var: 'civic.clean_criminal_record', op: 'EQ', val: true },
                    { var: 'civic.no_expulsion_order', op: 'EQ', val: true },
                ]
            };
            expect(evaluateRule(profile, condition)).toBe(true);
        });

        it('rejects non-refugee for refugee card', () => {
            const profile = {
                identity: { age: 35, nationality_group: 'NON_EU' },
                civic: { clean_criminal_record: true, no_expulsion_order: true },
            };
            const condition: RuleCondition = {
                AND: [
                    { var: 'identity.age', op: 'GTE', val: 18 },
                    { var: 'identity.nationality_group', op: 'EQ', val: 'REFUGEE' },
                    { var: 'civic.clean_criminal_record', op: 'EQ', val: true },
                ]
            };
            expect(evaluateRule(profile, condition)).toBe(false);
        });

        it('matches age range condition (jeune au pair 18-30)', () => {
            const profile = { identity: { age: 22 } };
            const condition: RuleCondition = {
                AND: [
                    { var: 'identity.age', op: 'GTE', val: 18 },
                    { var: 'identity.age', op: 'LTE', val: 30 },
                ]
            };
            expect(evaluateRule(profile, condition)).toBe(true);

            const profileTooOld = { identity: { age: 35 } };
            expect(evaluateRule(profileTooOld, condition)).toBe(false);
        });

        it('matches naturalisation by marriage (4 years + B1)', () => {
            const profile = {
                identity: { age: 30 },
                family: { spouse_nationality: 'FRENCH', community_of_life: true, marriage_duration_years: 5 },
                integration: { french_level: 'B1' },
                civic: { clean_criminal_record: true, no_expulsion_order: true },
            };
            const condition: RuleCondition = {
                AND: [
                    { var: 'family.spouse_nationality', op: 'EQ', val: 'FRENCH' },
                    { var: 'family.community_of_life', op: 'EQ', val: true },
                    { var: 'family.marriage_duration_years', op: 'GTE', val: 4 },
                    { var: 'integration.french_level', op: 'GTE', val: 'B1' },
                    { var: 'civic.clean_criminal_record', op: 'EQ', val: true },
                ]
            };
            expect(evaluateRule(profile, condition)).toBe(true);
        });
    });

    // ─── Edge cases ───────────────────────────────────────────────

    describe('edge cases', () => {
        it('handles empty condition object', () => {
            expect(evaluateRule({ identity: { age: 25 } }, {})).toBe(false);
        });

        it('handles empty AND array (vacuous truth)', () => {
            expect(evaluateRule({}, { AND: [] })).toBe(true);
        });

        it('handles empty OR array (vacuous falsity)', () => {
            expect(evaluateRule({}, { OR: [] })).toBe(false);
        });
    });
});
