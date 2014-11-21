package rvm;
import java.util.concurrent.*;
import java.util.concurrent.locks.*;
import java.util.*;
import java.lang.ref.*;
import com.runtimeverification.rvmonitor.java.rt.*;
import com.runtimeverification.rvmonitor.java.rt.ref.*;
import com.runtimeverification.rvmonitor.java.rt.table.*;
import com.runtimeverification.rvmonitor.java.rt.tablebase.AbstractIndexingTree;
import com.runtimeverification.rvmonitor.java.rt.tablebase.SetEventDelegator;
import com.runtimeverification.rvmonitor.java.rt.tablebase.TableAdopter.Tuple2;
import com.runtimeverification.rvmonitor.java.rt.tablebase.TableAdopter.Tuple3;
import com.runtimeverification.rvmonitor.java.rt.tablebase.IDisableHolder;
import com.runtimeverification.rvmonitor.java.rt.tablebase.IMonitor;
import com.runtimeverification.rvmonitor.java.rt.tablebase.DisableHolder;
import com.runtimeverification.rvmonitor.java.rt.tablebase.TerminatedMonitorCleaner;
import java.util.concurrent.atomic.AtomicInteger;

final class PubMonitor_Set extends com.runtimeverification.rvmonitor.java.rt.tablebase.AbstractMonitorSet<PubMonitor> {

	PubMonitor_Set(){
		this.size = 0;
		this.elements = new PubMonitor[4];
	}
	final void event_publish(Integer report, int time) {
		int numAlive = 0 ;
		for(int i = 0; i < this.size; i++){
			PubMonitor monitor = this.elements[i];
			if(!monitor.isTerminated()){
				elements[numAlive] = monitor;
				numAlive++;

				final PubMonitor monitorfinalMonitor = monitor;
				monitor.Prop_1_event_publish(report, time);
				if(monitorfinalMonitor.Prop_1_Category_violation) {
					monitorfinalMonitor.Prop_1_handler_violation();
				}
			}
		}
		for(int i = numAlive; i < this.size; i++){
			this.elements[i] = null;
		}
		size = numAlive;
	}
	final void event_approve(Integer report, int time) {
		int numAlive = 0 ;
		for(int i = 0; i < this.size; i++){
			PubMonitor monitor = this.elements[i];
			if(!monitor.isTerminated()){
				elements[numAlive] = monitor;
				numAlive++;

				final PubMonitor monitorfinalMonitor = monitor;
				monitor.Prop_1_event_approve(report, time);
				if(monitorfinalMonitor.Prop_1_Category_violation) {
					monitorfinalMonitor.Prop_1_handler_violation();
				}
			}
		}
		for(int i = numAlive; i < this.size; i++){
			this.elements[i] = null;
		}
		size = numAlive;
	}
}

class PubMonitor extends com.runtimeverification.rvmonitor.java.rt.tablebase.AbstractAtomicMonitor implements Cloneable, com.runtimeverification.rvmonitor.java.rt.RVMObject {
	protected Object clone() {
		try {
			PubMonitor ret = (PubMonitor) super.clone();
			return ret;
		}
		catch (CloneNotSupportedException e) {
			throw new InternalError(e.toString());
		}
	}

	Integer report;
	int time;

	static final int Prop_1_transition_publish[] = {2, 0, 3, 3};;
	static final int Prop_1_transition_approve[] = {1, 1, 3, 3};;

	volatile boolean Prop_1_Category_violation = false;

	private final AtomicInteger pairValue;

	PubMonitor() {
		this.pairValue = new AtomicInteger(this.calculatePairValue(-1, 0) ) ;

	}

	@Override public final int getState() {
		return this.getState(this.pairValue.get() ) ;
	}
	@Override public final int getLastEvent() {
		return this.getLastEvent(this.pairValue.get() ) ;
	}
	private final int getState(int pairValue) {
		return (pairValue & 3) ;
	}
	private final int getLastEvent(int pairValue) {
		return (pairValue >> 2) ;
	}
	private final int calculatePairValue(int lastEvent, int state) {
		return (((lastEvent + 1) << 2) | state) ;
	}

	private final int handleEvent(int eventId, int[] table) {
		int nextstate;
		while (true) {
			int oldpairvalue = this.pairValue.get() ;
			int oldstate = this.getState(oldpairvalue) ;
			nextstate = table [ oldstate ];
			int nextpairvalue = this.calculatePairValue(eventId, nextstate) ;
			if (this.pairValue.compareAndSet(oldpairvalue, nextpairvalue) ) {
				break;
			}
		}
		return nextstate;
	}

	final boolean Prop_1_event_publish(Integer report, int time) {
		{
			this.report=report;
			this.time=time;
		}

		int nextstate = this.handleEvent(0, Prop_1_transition_publish) ;
		this.Prop_1_Category_violation = nextstate == 2;

		return true;
	}

	final boolean Prop_1_event_approve(Integer report, int time) {
		{
			this.report=report;
			this.time=time;
		}

		int nextstate = this.handleEvent(1, Prop_1_transition_approve) ;
		this.Prop_1_Category_violation = nextstate == 2;

		return true;
	}

	final void Prop_1_handler_violation (){
		{ System.out.println("should not publish financial report "+this.report+" without pre-approval");}

	}

	final void reset() {
		this.pairValue.set(this.calculatePairValue(-1, 0) ) ;

		Prop_1_Category_violation = false;
	}

	// RVMRef_report was suppressed to reduce memory overhead

	//alive_parameters_0 = [Integer report]
	boolean alive_parameters_0 = true;

	@Override
	protected final void terminateInternal(int idnum) {
		int lastEvent = this.getLastEvent();

		switch(idnum){
			case 0:
			alive_parameters_0 = false;
			break;
		}
		switch(lastEvent) {
			case -1:
			return;
			case 0:
			//publish
			//alive_report
			if(!(alive_parameters_0)){
				RVM_terminated = true;
				return;
			}
			break;

			case 1:
			//approve
			//alive_report
			if(!(alive_parameters_0)){
				RVM_terminated = true;
				return;
			}
			break;

		}
		return;
	}

	public static int getNumberOfEvents() {
		return 2;
	}

	public static int getNumberOfStates() {
		return 4;
	}

}

public final class PubRuntimeMonitor implements com.runtimeverification.rvmonitor.java.rt.RVMObject {
	private static com.runtimeverification.rvmonitor.java.rt.map.RVMMapManager PubMapManager;
	static {
		PubMapManager = new com.runtimeverification.rvmonitor.java.rt.map.RVMMapManager();
		PubMapManager.start();
	}

	// Declarations for the Lock
	static final ReentrantLock Pub_RVMLock = new ReentrantLock();
	static final Condition Pub_RVMLock_cond = Pub_RVMLock.newCondition();

	private static boolean Pub_activated = false;

	// Declarations for Indexing Trees
	private static Object Pub_report_Map_cachekey_report;
	private static PubMonitor Pub_report_Map_cachevalue;
	private static final MapOfMonitor<PubMonitor> Pub_report_Map = new MapOfMonitor<PubMonitor>(0) ;

	public static int cleanUp() {
		int collected = 0;
		// indexing trees
		collected += Pub_report_Map.cleanUpUnnecessaryMappings();
		return collected;
	}

	// Removing terminated monitors from partitioned sets
	static {
		TerminatedMonitorCleaner.start() ;
	}
	// Setting the behavior of the runtime library according to the compile-time option
	static {
		RuntimeOption.enableFineGrainedLock(false) ;
	}

	public static final void publishEvent(Integer report, int time) {
		Pub_activated = true;
		while (!Pub_RVMLock.tryLock()) {
			Thread.yield();
		}

		CachedWeakReference wr_report = null;
		MapOfMonitor<PubMonitor> matchedLastMap = null;
		PubMonitor matchedEntry = null;
		boolean cachehit = false;
		if ((report == Pub_report_Map_cachekey_report) ) {
			matchedEntry = Pub_report_Map_cachevalue;
			cachehit = true;
		}
		else {
			wr_report = new CachedWeakReference(report) ;
			{
				// FindOrCreateEntry
				MapOfMonitor<PubMonitor> itmdMap = Pub_report_Map;
				matchedLastMap = itmdMap;
				PubMonitor node_report = Pub_report_Map.getNodeEquivalent(wr_report) ;
				matchedEntry = node_report;
			}
		}
		// D(X) main:1
		if ((matchedEntry == null) ) {
			if ((wr_report == null) ) {
				wr_report = new CachedWeakReference(report) ;
			}
			// D(X) main:4
			PubMonitor created = new PubMonitor() ;
			matchedEntry = created;
			matchedLastMap.putNode(wr_report, created) ;
		}
		// D(X) main:8--9
		final PubMonitor matchedEntryfinalMonitor = matchedEntry;
		matchedEntry.Prop_1_event_publish(report, time);
		if(matchedEntryfinalMonitor.Prop_1_Category_violation) {
			matchedEntryfinalMonitor.Prop_1_handler_violation();
		}

		if ((cachehit == false) ) {
			Pub_report_Map_cachekey_report = report;
			Pub_report_Map_cachevalue = matchedEntry;
		}

		Pub_RVMLock.unlock();
	}

	public static final void approveEvent(Integer report, int time) {
		Pub_activated = true;
		while (!Pub_RVMLock.tryLock()) {
			Thread.yield();
		}

		CachedWeakReference wr_report = null;
		MapOfMonitor<PubMonitor> matchedLastMap = null;
		PubMonitor matchedEntry = null;
		boolean cachehit = false;
		if ((report == Pub_report_Map_cachekey_report) ) {
			matchedEntry = Pub_report_Map_cachevalue;
			cachehit = true;
		}
		else {
			wr_report = new CachedWeakReference(report) ;
			{
				// FindOrCreateEntry
				MapOfMonitor<PubMonitor> itmdMap = Pub_report_Map;
				matchedLastMap = itmdMap;
				PubMonitor node_report = Pub_report_Map.getNodeEquivalent(wr_report) ;
				matchedEntry = node_report;
			}
		}
		// D(X) main:1
		if ((matchedEntry == null) ) {
			if ((wr_report == null) ) {
				wr_report = new CachedWeakReference(report) ;
			}
			// D(X) main:4
			PubMonitor created = new PubMonitor() ;
			matchedEntry = created;
			matchedLastMap.putNode(wr_report, created) ;
		}
		// D(X) main:8--9
		final PubMonitor matchedEntryfinalMonitor = matchedEntry;
		matchedEntry.Prop_1_event_approve(report, time);
		if(matchedEntryfinalMonitor.Prop_1_Category_violation) {
			matchedEntryfinalMonitor.Prop_1_handler_violation();
		}

		if ((cachehit == false) ) {
			Pub_report_Map_cachekey_report = report;
			Pub_report_Map_cachevalue = matchedEntry;
		}

		Pub_RVMLock.unlock();
	}

}
