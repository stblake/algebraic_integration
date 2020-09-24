with(Algebraic):

findElementaryIntegral := proc(maxint, maxdeg, raddeg, radexpt, x, ngoal)

	local ngiveup, nsincelast, totaltime, nfound, ntrials, nrepeats, num, den, rad, integrand, integral, radsqf, fac, r;

	Seed := randomize();

	ngiveup := 2500;

	randinrange := rand(-maxint..maxint);
	negrandinrange := rand(-maxint..-1);
	posrandinrange := rand(1..maxint);
	randf := rand(0.0..1.0);

	interface(prettyprint=0);

	found := [];
	nfound := 0;
	ntrials := 0;
	nrepeats := 0;
	nsincelast := 0;
	while (nfound < ngoal) do

		num := [seq(randinrange()*x^k, k=0..maxdeg)];
		num := convert(num, `+`);
		den := [seq(randinrange()*x^k, k=0..maxdeg)];
		den := convert(den, `+`);
		rad := [seq(randinrange()*x^k, k=1..raddeg-1)];
		rad := convert(rad, `+`);
		if randf() < 0.5 then
			rad := x^raddeg + negrandinrange() + rad;
		else
			rad := x^raddeg + posrandinrange() + rad;
		end if;
	
		if num*den*rad = 0 then next end if;  

		radsqf := Squarefree(rad)[2];
		m := 1/radexpt;

		if nops(radsqf) = 1 then
			if degree(radsqf[1][1],x) = 1 then next end if;
		end if;

		bad := false;
		for fac in radsqf do
			r := fac[2] mod m;
			if r = 0 then 
				bad := true;
			end if;
		end do;

		if bad then next end if;

		integrand := normal(num/den)/(rad)^radexpt; 

		if evalb(integrand in found) then 
			nrepeats += 1;
			if nrepeats > 4 then 
				break
			end if;
			next 
		end if;

		ntrials += 1;
#		print ("integrand = ", integrand);
		t0 := Now(ProcessClock);
		integral := int(convert(integrand, RootOf), x);
		t1 := Now(ProcessClock);
		totaltime := totaltime + t1 - t0;
#		print("integral = ", integral);

		if not has(integral,[int,EllipticPi,EllipticE,EllipticF])  then
			nfound += 1;
			nsincelast := 0;
			found := [op(found), integrand];
			print("integrand = ", integrand);
			print("integral  = ", convert(integral, radical));
		else
			nsincelast += 1;
		end if;

		if nsincelast > ngiveup then break end if;

		if ntrials mod 100 = 0 then
		   print("ntrials = ", ntrials, ", nfound = ", nfound, ", % elementary = ", evalf(100.0*nfound/ntrials));
		end if;
	end;

	print("FINISHED! params = ", maxint, maxdeg, raddeg, radexpt, x, ngoal, " ntrials = ", ntrials, ", nfound = ", nfound, ", % elementary = ", evalf(100.0*nfound/ntrials), ", mean time = ", evalf(totaltime/ntrials));
end proc;

