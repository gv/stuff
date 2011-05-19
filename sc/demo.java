package vg.scother;

import java.lang.Thread;
import java.util.Scanner;
import vg.Sc.NetworkNode;

public class demo implements NetworkNode.User {
	public void handleOtherNodePresence(NetworkNode.Status status) {

	}

	public static void main(String[] args) throws Exception {
		NetworkNode n = new NetworkNode(new demo());
		Scanner sc = new Scanner(System.in);
		sc.nextLine();
		n.quit();
	}
}

