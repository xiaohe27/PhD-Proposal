import rvm.PubRuntimeMonitor;

public class TestPub {
	public TestPub(){
	}

	public static void main(String[] args){
		PubRuntimeMonitor.approveEvent(2, 0);
		PubRuntimeMonitor.approveEvent(3, 0);
		PubRuntimeMonitor.publishEvent(3, 5);
		PubRuntimeMonitor.publishEvent(8, 5);

//PubRuntimeMonitor.publishEvent(2, 52);
	}

}
