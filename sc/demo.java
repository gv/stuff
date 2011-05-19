package vg.scother;

import java.lang.Thread;

import vg.Sc.NetworkNode;

public class demo implements NetworkNode.User {
	public void handleOtherNodePresence(NetworkNode.Status status) {

	}

	public static void main(String[] args) throws Exception {
		NetworkNode n = new NetworkNode(new demo());
		Thread.sleep(1000000);
	}
}

