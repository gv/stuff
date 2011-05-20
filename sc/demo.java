package vg.scother;

import java.lang.Thread;
import java.util.Scanner;
import vg.Sc.NetworkNode;

public class demo implements NetworkNode.User {
	public void handleOtherNodePresence(NetworkNode.Status status) {
		System.out.println("present: " + status.name);
	}

	public void handleOtherNodeQuit(NetworkNode.Status status) {
		System.out.println("quit: " + status.name);
	}

	public static void main(String[] args) throws Exception {
		NetworkNode n = new NetworkNode(new demo());
		Scanner sc = new Scanner(System.in);
		sc.nextLine();
		n.quit();
	}
}

