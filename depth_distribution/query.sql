create or replace function depthcat(depth double precision)
returns int as $$
declare categories int[] := array[0, 50, 100, 150, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1200, 1400, 1600, 1800, 2000, 2200, 2400, 2600, 2800, 3000, 3200, 3400, 3600, 3800, 4000, 4200, 4400, 4600, 4800, 5000, 5200, 5400, 5600, 5800, 6000, 7000, 8000, 9000, 10000, 11000];
begin
	for i in array_lower(categories, 1) .. array_upper(categories, 1)
	loop
		if depth < categories[i] then
			return i - 1; end if;
	end loop;
	return NULL;
end;
$$
language 'plpgsql';

select
	depthcat(depth) as depth,
	depthcat(bottomdepth) as bottomdepth,
	count(*) as records,
	count(distinct(valid_id)) as taxa
from obis.drs
left join obis.positions on positions.id = drs.position_id
where depth >= 0 and bottomdepth >= 0 and depthcat(depth) is not null and depthcat(bottomdepth) is not null
group by depthcat(depth), depthcat(bottomdepth);

select
	depthcat(depth) as depth,
	depthcat(bottomdepth) as bottomdepth,
	count(*) as records,
	count(distinct(valid_id)) as taxa
from obis.drs
left join obis.positions on positions.id = drs.position_id
where depth >= 0 and bottomdepth >= 0 and depthcat(depth) is not null and depthcat(bottomdepth) is not null
group by depthcat(depth), depthcat(bottomdepth);

select
	depth,
	bottomdepth,
	depthcat(depth),
	depthcat(bottomdepth)
from obis.drs
left join obis.positions
on positions.id = drs.position_id
where depth >= 1800 and depth <= 2000 and bottomdepth >= 4400 and bottomdepth <= 4600