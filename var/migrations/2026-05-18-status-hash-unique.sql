-- Phase C migration: enforce idempotence at the weaving_status level
-- so the bind-parameter INSERT ... ON CONFLICT (ust_hash) DO NOTHING
-- path in repository_status:insert/3 is safe.
--
-- Pre-flight check (run before this migration):
--
--   SELECT ust_hash, count(*) AS dup
--   FROM weaving_status
--   GROUP BY ust_hash
--   HAVING count(*) > 1
--   ORDER BY dup DESC
--   LIMIT 10;
--
-- If rows are returned, dedup first by keeping the lowest ust_id per
-- hash:
--
--   DELETE FROM weaving_status s1
--   USING  weaving_status s2
--   WHERE  s1.ust_hash = s2.ust_hash
--     AND  s1.ust_id > s2.ust_id;
--
-- Then run:

ALTER TABLE weaving_status
    ADD CONSTRAINT weaving_status_ust_hash_unique UNIQUE (ust_hash);
