-- Phase C migration: enforce idempotence at the publication level so
-- the bind-parameter INSERT ... ON CONFLICT (hash) DO NOTHING path in
-- repository_publication:insert/2 is safe.
--
-- Pre-flight check (run before this migration):
--
--   SELECT hash, count(*) AS dup
--   FROM publication
--   GROUP BY hash
--   HAVING count(*) > 1
--   ORDER BY dup DESC
--   LIMIT 10;
--
-- If rows are returned, dedup first by keeping the lowest legacy_id
-- per hash:
--
--   DELETE FROM publication p1
--   USING  publication p2
--   WHERE  p1.hash = p2.hash
--     AND  p1.ctid > p2.ctid;
--
-- Then run:

ALTER TABLE publication
    ADD CONSTRAINT publication_hash_unique UNIQUE (hash);
