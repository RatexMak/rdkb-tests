/*
 * Copyright 2021 Comcast Cable Communications Management, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.automatics.rdkb.tests.system;

import java.util.ArrayList;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.BroadBandResultObject;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandTraceConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadBandPreConditionUtils;
import com.automatics.rdkb.utils.BroadBandSystemUtils;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.rdkb.utils.wifi.BroadBandWiFiUtils;
import com.automatics.rdkb.utils.wifi.connectedclients.BroadBandConnectedClientUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;

public class BroadBandSystemWifiTests extends AutomaticsTestBase {

    /**
     * Verify Xconf default value,Hardware make/model,Linux kernel upgrade version
     * <ol>
     * <li>Verify WEBPA command to get the XCONF client firmware download default value.</li>
     * <li>Verify Hardware Make/Model of the device in Xconf logs</li>
     * <li>Verify Linux Kernel Upgrade Version.</li>
     * </ol>
     * 
     * @param device
     *            {@link Dut}
     * @author Joseph M
     */
    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true)
    @TestDetails(testUID = "TC-RDKB-SYSTEM-5007",  testDecription = "Verify Xconf default value,Hardware make/model,Linux kernel upgrade version")
    public void testToVerifyHardwareAndLinuxVersion(Dut device) {
	// Variable Declaration begins
	String testCaseId = "";
	String stepNum = "";
	String errorMessage = "";
	boolean status = false;
	String response = null;
	testCaseId = "TC-RDKB-SYSTEM-507";

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-SYSTEM-5007");
	LOGGER.info("TEST DESCRIPTION: Verify Xconf default value,Hardware make/model,Linux kernel upgrade version");

	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Verify WEBPA command to get the XCONF client firmware download  default value.");
	LOGGER.info("2. Verify Hardware  Make/Model of the device in xconf logs");
	LOGGER.info("3. Verify Linux Kernel Upgrade Version.");
	LOGGER.info("#######################################################################################");

	LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	LOGGER.info("PRE-CONDITION STEPS");
	LOGGER.info("#######################################################################################");
	LOGGER.info("PRE-CONDITION : DESCRIPTION : Reboot the device.");
	LOGGER.info("PRE-CONDITION : ACTION :Reboot the device using command: /sbin/reboot.");
	LOGGER.info("PRE-CONDITION : EXPECTED :  Device should be rebooted.");
	LOGGER.info("#######################################################################################");
	status = CommonUtils.rebootAndWaitForIpAcquisition(tapEnv, device);
	errorMessage = "Unable to reboot the device.";
	if (status) {
	    LOGGER.info("PRE-CONDITION : ACTUAL : Pre condition executed successfully");
	} else {
	    LOGGER.error("PRE-CONDITION : ACTUAL : Pre condition failed");
	    throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION :FAILED : "
		    + errorMessage);
	}
	LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");
	try {
	    stepNum = "S1";
	    errorMessage = "The XCONF client firmware download check default value is not as false.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION : Verify WEBPA command to get the XCONF client firmware download  default value");
	    LOGGER.info("STEP 1: ACTION : Execute the Below Command:curl -X GET -H 'Authorization: Bearer <SAT TOKEN>' -H 'content-type:application/json' -H 'X-Webpa-Atomic:true' -k -i https://<url:port>/api/v2/device/mac:<MAC Address>/config?names=Device.X_COMCAST-COM_Xcalibur.Client.xconfCheckNow");
	    LOGGER.info("STEP 1: EXPECTED : The output value false confirms the XCONF client firmware download check default value");
	    LOGGER.info("**********************************************************************************");
	    String xconfDefaultValue = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_FOR_TRIGGERING_XCONF_CDL);
	    LOGGER.info("Xconf Default value  is = " + xconfDefaultValue);
	    if (CommonMethods.isNotNull(xconfDefaultValue)
		    && xconfDefaultValue.equalsIgnoreCase(BroadBandTestConstants.FALSE)) {
		status = true;
	    }
	    if (status) {
		LOGGER.info("STEP 1: ACTUAL : XCONF client firmware download check default value retrieved is false");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "S2";
	    errorMessage = "Unable to get the Hardware  Make/Model of the device in logs.";
	    status = false;
	    boolean modelInXconfStatus = false;
	    String command = null;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 2: DESCRIPTION : Verify Hardware  Make/Model of the device in xconf logs.");
	    LOGGER.info("STEP 2: ACTION : Get the Hardware Make/Model by using below command 'grep -i \"XCONF SCRIPT : MODEL\" /rdklogs/logs/ArmConsolelog.txt.0' for atom devices and 'grep -i \"XCONF SCRIPT\" /rdklogs/logs/Consolelog.txt.0' for arm devices and 'grep -i \"XCONF SCRIPT\" /rdklogs/logs/xconf.txt.0' for other devices");
	    LOGGER.info("STEP 2: EXPECTED : Hardware  Make/Model of the device should be displayed .");
	    LOGGER.info("**********************************************************************************");
	    long startTime = System.currentTimeMillis();
	    if (CommonMethods.isAtomSyncAvailable(device, tapEnv)) {
		command = BroadBandCommandConstants.XCONF_CURRENT_MODEL_ARM_CONSOLE_LOGS;
	    } 
	else{
		command = BroadBandCommandConstants.XCONF_CURRENT_MODEL;
	    }
	    do {
		response = tapEnv.executeCommandUsingSsh(device, command);
		if (CommonMethods.isNotNull(response)) {
			modelInXconfStatus = CommonUtils.patternSearchFromTargetString(response, device.getModel());
			LOGGER.info("Model Name in log status is" + modelInXconfStatus);
		}
		status = modelInXconfStatus;
	    } while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.FIFTEEN_MINUTES_IN_MILLIS
		    && !status
		    && BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
	    if (status) {
		LOGGER.info("STEP 2: ACTUAL : Able to get the Hardware version and Model of the device in xconf logs ");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "S3";
	    errorMessage = "Linux kernel version is not displayed in Arm Console";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 3: DESCRIPTION : Verify Linux Kernel Upgrade Version in ArmConsole");
	    LOGGER.info("STEP 3: ACTION : Get the Linux Kernel Upgrade Version by using the command 'uname -r'");
	    LOGGER.info("STEP 3: EXPECTED : Linux Kernel Version  (LTS version) should be displayed in ArmConsole");
	    LOGGER.info("**********************************************************************************");
	    response = tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_LINUX_VERSION);
	    LOGGER.info("Response for linux kernel upgrade is" + response);
	    status = CommonUtils.patternSearchFromTargetString(response.trim(),
		    tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_LINUX_KERNEL_VERSION).trim());
	    if (status) {
		LOGGER.info("STEP 3: ACTUAL : Linux Kernel Version is verified successfully.");
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils
		    .updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage, true);
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-SYSTEM-5007");
    }
    
	/**
	 * <ol>
	 * <li>STEP 1: Verify the version of ipset</li>
	 * <li>STEP 2: Create and verify ip blacklist for hash:ip</li>
	 * <li>STEP 3: Add ip and url from phishtank site</li>
	 * <li>STEP 4: Verify the destination ip reachability using icmp</li>
	 * <li>STEP 5: verify the destination ip reachability using UDP</li>
	 * <li>STEP 6: verify the destination ip reachability using TCP</li>
	 * <li>STEP 7: Add ip and verify ipset blacklist</li>
	 * <li>STEP 8: Verify the command to drop the blacklisted ips</li>
	 * <li>STEP 9: Verify the list of iptable rules for blacklist</li>
	 * <li>STEP 10: Verify the destination ip reachability using icmp</li>
	 * <li>STEP 11: verify the destination ip reachability using UDP</li>
	 * <li>STEP 12: verify the destination ip reachability using TCP</li>
	 * <li>STEP 13: flush and verify the ips blacklisted</li>
	 * <li>STEP 14: verify the ipset list after flusing the ipset list</li>
	 * <li>STEP 15: Verify the destination ip reachability using icmp</li>
	 * <li>STEP 16: verify the destination ip reachability using UDP</li>
	 * <li>STEP 17: verify the destination ip reachability using TCP</li>
	 * </ol>
	 * 
	 * @author Sumathi Gunasekaran
	 * @refactor Athira
	 * 
	 * @param device {@link Dut}
	 */

	@Test(enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, groups = {
			BroadBandTestGroup.WEBPA }, dataProviderClass = AutomaticsTapApi.class)
	@TestDetails(testUID = "TC-RDKB-SYSTEM-IPSET-1008")

	public void verifyUpgradeIpsetVersion(Dut device) {
		// String to store the test case ID
		String testId = "TC-RDKB-SYSTEM-IPSET-008";
		// String to store the test step number
		String testStepNumber = "s1";
		// String to store the error message
		String errorMessage = null;
		// stores the test status
		boolean status = false;
		// stores the response
		String response = null;
		// Array list to store phish Ip Address
		final ArrayList<String> phishIPAddress = new ArrayList<String>();
		// Array list to store phish url
		final ArrayList<String> phishDomainName = new ArrayList<String>();
		// variable to store counter value
		ArrayList<Boolean> statusArrayList = new ArrayList<Boolean>();
		// variable to store counter value
		ArrayList<Boolean> pingStatusArrayList = new ArrayList<Boolean>();
		// variable to store counter value
		ArrayList<Boolean> digStatusArrayList = new ArrayList<Boolean>();
		// variable to store counter value
		ArrayList<Boolean> wgetStatusArrayList = new ArrayList<Boolean>();
		int counter = 0;
		// String variable String commandToExecute
		String commandToExecute = null;
		// String variable to hold ping response array
		String pingResponseArray[] = null;
		try {
			LOGGER.info("******************* STARTING PRE-CONFIGURATIONS ********************");
			LOGGER.info("PRE-CONDITION STEPS");
			LOGGER.info(
					"PRE-CONDITION : DESCRIPTION : Verify whether any device is connected through Ethernet to the Broadband Gateway");
			LOGGER.info(
					"PRE-CONDITION : EXPECTED : Broadband Gateway should be connected to the client device through ethernet");
			Dut clientSettop = BroadBandConnectedClientUtils.getEthernetConnectedClient(tapEnv, device);
			errorMessage = "Unable to obtain a ethernet connected client!";
			if (clientSettop == null) {
				throw new TestException(
						BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.PRE_CONDITION_ERROR,
								BroadBandTestConstants.SINGLE_SPACE_CHARACTER, errorMessage));
			}

			LOGGER.info("******************* COMPLETED PRE-CONFIGURATIONS ********************");

			LOGGER.info("**************************************************************************");
			LOGGER.info("STARTING TEST CASE: TC-RDKB-SYSTEM-IPSET-1008");

			// Verify the version of ipset
			errorMessage = "Failed to execute command:" + BroadBandCommandConstants.CMD_IPSET_VERSION;
			LOGGER.info("******************************************************************");
			LOGGER.info("STEP 1 : DESCRIPTION : Verify the version of ipset");
			LOGGER.info("STEP 1 : ACTION : Execute the below Command: ipset -v");
			LOGGER.info(
					"STEP 1 : EXPECTED : The command should execute successfully and output is, ipset v6.34, protocol version: 6");
			LOGGER.info("******************************************************************");
			response = tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_IPSET_VERSION);

			status = CommonMethods.isNotNull(response) && CommonUtils.isGivenStringAvailableInCommandOutput(response,
					BroadBandTraceConstants.LOG_MESSAGE_IPSET_VERSION);
			errorMessage = "Failed to get the ipset version as:" + BroadBandTraceConstants.LOG_MESSAGE_IPSET_VERSION;
			if (status) {
				LOGGER.info("STEP 1: ACTUAL : Succcessfully verified ipset version as:"
						+ BroadBandTraceConstants.LOG_MESSAGE_IPSET_VERSION);
			} else {
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}
			LOGGER.info("******************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			// Create and verify ip blacklist for hash:ip
			testStepNumber = "s2";
			errorMessage = null;
			status = false;
			LOGGER.info("******************************************************************");

			LOGGER.info("STEP 2 : DESCRIPTION : Create and verify ip blacklist for hash:ip");
			LOGGER.info("STEP 2 : ACTION : Execute Command: ipset create ip_blacklist hash:ip");
			LOGGER.info("STEP 2 : EXPECTED : The Command should execute successfully.");
			LOGGER.info("******************************************************************");
			tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_CREATE_BLACKLIST_IPSET);
			response = tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_LIST_IPSET);
			status = CommonMethods.isNotNull(response) && CommonUtils.isGivenStringAvailableInCommandOutput(response,
					BroadBandTraceConstants.LOG_MESSAGE_BLACKLIST);
			errorMessage = "Failed to create ip blacklist for hash:ip";
			if (status) {
				LOGGER.info("STEP 2: ACTUAL : Succcessfully created ip blacklist for hash:ip");
			} else {
				LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
			}
			LOGGER.info("******************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			// Add ip and url from phishtank site
			testStepNumber = "s3";
			errorMessage = null;
			status = false;
			LOGGER.info("******************************************************************");
			LOGGER.info("STEP 3 : DESCRIPTION : Add ip and url from phishtank site");
			LOGGER.info("STEP 3 : ACTION : Execute command: nslookup url");
			LOGGER.info("STEP 3 : EXPECTED : Ip and website url should be added in different lists");
			LOGGER.info("******************************************************************");
			errorMessage = "Failed to reach phish tank site";

			for (String phishTankUrl : BroadBandTestConstants.LIST_OF_PHISHED_WEBSITES) {
				errorMessage = "Failed to get IpAddress and DomainName from nslookup command";
				pingResponseArray = BroadBandSystemUtils.retrieveIPAddressAndDomainName(clientSettop, tapEnv,
						phishTankUrl);
				if (CommonMethods.isNotNull(pingResponseArray[BroadBandTestConstants.CONSTANT_0])
						&& (CommonMethods.isNotNull(pingResponseArray[BroadBandTestConstants.CONSTANT_1]))) {
					phishIPAddress.add(pingResponseArray[BroadBandTestConstants.CONSTANT_0]);
					phishDomainName.add(pingResponseArray[BroadBandTestConstants.CONSTANT_1]);
				}
			}
			if (phishIPAddress.size() == phishDomainName.size()) {
				status = true;
			}
			if (status) {
				LOGGER.info("STEP 3: ACTUAL : Succcessfully added ip and url to the array list");
			} else {
				LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
			}
			LOGGER.info("******************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			// Verify the destination ip reachability using icmp
			testStepNumber = "s4";
			errorMessage = null;
			status = false;
			statusArrayList.clear();
			LOGGER.info("******************************************************************");
			LOGGER.info("STEP 4  : DESCRIPTION : Verify the destination ip reachability using icmp");
			LOGGER.info("STEP 4 : ACTION : Execute Command: ping -c 2 ipAddress");
			LOGGER.info("STEP 4  : EXPECTED : Ping should  reach to the destination ip.");
			LOGGER.info("******************************************************************");
			for (String ipAddress : phishIPAddress) {
				commandToExecute = CommonUtils.concatStringUsingStringBuffer(
						BroadBandCommandConstants.CMD_PING_COUNT_TWO, BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
						ipAddress);
				response = tapEnv.executeCommandOnOneIPClients(clientSettop, commandToExecute);
				status = CommonMethods.isNotNull(response)
						&& (!CommonUtils.isGivenStringAvailableInCommandOutput(response,
								BroadBandTraceConstants.PACKET_LOSS_HUNDRED_PERCENT));
				errorMessage = "ping has not reached the specified ip Address: " + ipAddress;
				LOGGER.info(status ? "ping has reached the ip Address:" + ipAddress : errorMessage + ipAddress);
				pingStatusArrayList.add(status);
			}
			status = (!pingStatusArrayList.isEmpty());
			if (status) {
				LOGGER.info("STEP 4: ACTUAL : ping has reached the ip Address successfully");
			} else {
				LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
			}
			LOGGER.info("******************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			// verify the destination ip reachability using UDP"
			testStepNumber = "s5";
			errorMessage = null;
			status = false;
			statusArrayList.clear();
			LOGGER.info("******************************************************************");
			LOGGER.info("STEP 5 : DESCRIPTION :verify the destination ip reachability using  UDP");
			LOGGER.info("STEP 5 : ACTION : Execute the below Command: dig url @ipaddress");
			LOGGER.info("STEP 5 : EXPECTED : dig should  reach the destination given");
			LOGGER.info("******************************************************************");
			for (counter = BroadBandTestConstants.CONSTANT_0; counter < phishDomainName.size(); counter++) {
				commandToExecute = CommonUtils.concatStringUsingStringBuffer(BroadBandCommandConstants.CMD_DIG,
						BroadBandTestConstants.SINGLE_SPACE_CHARACTER, phishDomainName.get(counter),
						BroadBandTestConstants.SINGLE_SPACE_CHARACTER, BroadBandTestConstants.SYMBOL_AT,
						phishIPAddress.get(counter));
				response = tapEnv.executeCommandOnOneIPClients(clientSettop, commandToExecute);
				status = CommonMethods.isNotNull(response) && (!CommonUtils
						.isGivenStringAvailableInCommandOutput(response, BroadBandTraceConstants.CONNECTION_TIMED_OUT));
				errorMessage = "dig command is blocked for the destination url";
				digStatusArrayList.add(status);
			}
			status = (!digStatusArrayList.isEmpty());

			if (status) {
				LOGGER.info("STEP 5: ACTUAL : dig command has reached the destination url successfully");
			} else {
				LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
			}
			LOGGER.info("******************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			// verify the destination ip reachability using TCP
			testStepNumber = "s6";
			errorMessage = null;
			status = false;
			statusArrayList.clear();
			LOGGER.info("******************************************************************");
			LOGGER.info("STEP 6 : DESCRIPTION : verify the destination ip reachability using TCP");
			LOGGER.info("STEP 6 : ACTION : Execute Below Command: wget http:url");
			LOGGER.info("STEP 6 : EXPECTED : wget should  reach the destination given");
			LOGGER.info("******************************************************************");
			for (String domainName : phishDomainName) {
				commandToExecute = CommonUtils.concatStringUsingStringBuffer(BroadBandCommandConstants.CMD_WGET_HTTP,
						domainName);
				response = tapEnv.executeCommandOnOneIPClients(clientSettop, commandToExecute);
				if (CommonMethods.isNotNull(response)) {
					status = (CommonUtils.isGivenStringAvailableInCommandOutput(response,
							BroadBandTraceConstants.HTTP_SUCCESS_CODE)
							|| CommonUtils.isGivenStringAvailableInCommandOutput(response,
									BroadBandTraceConstants.HTTP_FOUND_302_MESSAGE));
					errorMessage = "wget command is blocked for the destination url";
					wgetStatusArrayList.add(status);
				}

			}
			status = (!wgetStatusArrayList.isEmpty());
			if (status) {
				LOGGER.info("STEP 6: ACTUAL : wget command has reached the destination url successfully");
			} else {
				LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
			}
			LOGGER.info("******************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			// Add ip and verify ipset blacklist
			testStepNumber = "s7";
			errorMessage = null;
			status = false;
			LOGGER.info("******************************************************************");
			LOGGER.info("STEP 7 : DESCRIPTION : Add ip and verify ipset blacklist");
			LOGGER.info("STEP 7 : ACTION : Execute command: ipset add ip_blacklist ips");
			LOGGER.info("STEP 7 : EXPECTED : Added ip should be listed in ipset list.");
			LOGGER.info("******************************************************************");
			for (String ipAddress : phishIPAddress) {
				commandToExecute = CommonUtils.concatStringUsingStringBuffer(
						BroadBandCommandConstants.CMD_ADD_IP_TO_BLACKLIST,
						BroadBandTestConstants.SINGLE_SPACE_CHARACTER, ipAddress);
				errorMessage = "Failed to add ip to the blacklist";
				if (CommonMethods.isNull(tapEnv.executeCommandUsingSsh(device, commandToExecute))) {
					response = tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_LIST_IPSET);
					status = CommonMethods.isNotNull(response)
							&& CommonUtils.isGivenStringAvailableInCommandOutput(response, ipAddress);
					errorMessage = "Failed to add ip and verify ipset blacklist";
				}
			}
			if (status) {
				LOGGER.info("STEP 7: ACTUAL :Succcessfully added ip and verified ipset blacklist");
			} else {
				LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
			}
			LOGGER.info("******************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			// Verify the command to drop the blacklisted ips
			testStepNumber = "s8";
			errorMessage = null;
			status = false;
			LOGGER.info("******************************************************************");
			LOGGER.info("STEP 8 : DESCRIPTION : Verify the command to drop the blacklisted ips");
			LOGGER.info("STEP 8 : ACTION : Execute Command:" + BroadBandCommandConstants.CMD_DROP_IPSET_LIST);
			LOGGER.info("STEP 8 : EXPECTED : The command shold execute successfully to drop the blacklisted ips");
			LOGGER.info("******************************************************************");
			status = CommonMethods
					.isNull(tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_DROP_IPSET_LIST));
			errorMessage = "Failed to execute command to drop the blacklisted ips";

			if (status) {
				LOGGER.info("STEP 8: ACTUAL :Succcessfully executed the command to drop blacklisted ips");
			} else {
				LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
			}
			LOGGER.info("******************************************************************");

			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			// Verify the list of iptable rules for blacklist
			testStepNumber = "s9";
			errorMessage = null;
			status = false;
			LOGGER.info("******************************************************************");
			LOGGER.info("STEP 9  : DESCRIPTION : Verify the list of iptable rules for blacklist");
			LOGGER.info("STEP 9  : ACTION :Execute Command: " + BroadBandCommandConstants.CMD_LIST_IPSET_TABLE);
			LOGGER.info("STEP 9  : EXPECTED : The output should have the Drop message for black listed ipset");
			LOGGER.info("******************************************************************");
			response = tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_LIST_IPSET_TABLE);
			status = CommonMethods.isNotNull(response) && CommonUtils.isGivenStringAvailableInCommandOutput(response,
					BroadBandTestConstants.STRING_BLACKLIST);
			errorMessage = "Failed to list iptable rules for blacklist";
			if (status) {
				LOGGER.info("STEP 9: ACTUAL :Succcessfully listed the iptable rules for blacklist");
			} else {
				LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
			}
			LOGGER.info("******************************************************************");

			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			// Verify the destination ip reachability using icmp
			testStepNumber = "s10";
			errorMessage = null;
			status = false;
			LOGGER.info("******************************************************************");
			LOGGER.info("STEP 10  : DESCRIPTION : Verify the destination ip reachability using icmp");
			LOGGER.info("STEP 10 : ACTION : Execute Command: ping -c 2 ipaddress");
			LOGGER.info("STEP 10 : EXPECTED : Ping should not reach to the destination ip");
			LOGGER.info("******************************************************************");
			for (String ipAddress : phishIPAddress) {
				commandToExecute = CommonUtils.concatStringUsingStringBuffer(
						BroadBandCommandConstants.CMD_PING_COUNT_TWO, BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
						ipAddress);
				response = tapEnv.executeCommandOnOneIPClients(clientSettop, commandToExecute);
				status = CommonMethods.isNotNull(response) && CommonUtils.isGivenStringAvailableInCommandOutput(
						response, BroadBandTraceConstants.PACKET_LOSS_HUNDRED_PERCENT);
				LOGGER.info("ping is blocked for destination ip: " + ipAddress);
				errorMessage = "ping has reached the destination ip successfully";
				statusArrayList.add(status);
			}
			status = (!statusArrayList.contains(false));

			if (status) {
				LOGGER.info("STEP 10: ACTUAL :Successfully ping is blocked for given destination ips");
			} else {
				LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
			}
			LOGGER.info("******************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			// verify the destination ip reachability using UDP"
			testStepNumber = "s11";
			errorMessage = null;
			status = false;
			statusArrayList.clear();
			LOGGER.info("******************************************************************");
			LOGGER.info("STEP 11 : DESCRIPTION :verify the destination ip reachability using UDP");
			LOGGER.info("STEP 11 : ACTION : Execute the below Command: dig hostname @ipaddress");
			LOGGER.info("STEP 11 : EXPECTED : The dig command should not reach the host with the given ipaddress");
			LOGGER.info("******************************************************************");
			for (counter = BroadBandTestConstants.CONSTANT_0; counter < phishDomainName.size(); counter++) {
				commandToExecute = CommonUtils.concatStringUsingStringBuffer(BroadBandCommandConstants.CMD_DIG,
						BroadBandTestConstants.SINGLE_SPACE_CHARACTER, phishDomainName.get(counter),
						BroadBandTestConstants.SINGLE_SPACE_CHARACTER, BroadBandTestConstants.SYMBOL_AT,
						phishIPAddress.get(counter));
				response = tapEnv.executeCommandOnOneIPClients(clientSettop, commandToExecute);
				status = CommonMethods.isNotNull(response) && CommonUtils
						.isGivenStringAvailableInCommandOutput(response, BroadBandTraceConstants.CONNECTION_TIMED_OUT);
				errorMessage = "Dig Command has reached the host through blocked ipaddress";
				statusArrayList.add(status);
			}
			status = (!statusArrayList.contains(false));
			if (status) {
				LOGGER.info("STEP 11: ACTUAL :Dig is Successfully blocked for destination Host/IP");
			} else {
				LOGGER.error("STEP 11: ACTUAL : " + errorMessage);
			}
			LOGGER.info("******************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			// verify the destination ip reachability using TCP
			testStepNumber = "s12";
			errorMessage = null;
			status = false;
			statusArrayList.clear();
			LOGGER.info("******************************************************************");
			LOGGER.info("STEP 12 : DESCRIPTION : verify the destination ip reachability using TCP");
			LOGGER.info("STEP 12 : ACTION : Execute Below Command: wget url");
			LOGGER.info("STEP 12 : EXPECTED : wget should not reach the destination given hostname");
			LOGGER.info("******************************************************************");
			for (counter = BroadBandTestConstants.CONSTANT_0; counter < phishDomainName.size(); counter++) {
				commandToExecute = CommonUtils.concatStringUsingStringBuffer(BroadBandCommandConstants.CMD_WGET_HTTP,
						phishDomainName.get(counter));
				response = tapEnv.executeCommandOnOneIPClients(clientSettop, commandToExecute);
				if (CommonMethods.isNotNull(response)) {
					status = (!CommonUtils.isGivenStringAvailableInCommandOutput(response,
							BroadBandTraceConstants.HTTP_SUCCESS_CODE));
					errorMessage = "wget has reached the blocked  destination hostName";
					if (!status) {
						status = (!CommonUtils.isGivenStringAvailableInCommandOutput(response,
								phishIPAddress.get(counter)));
						statusArrayList.add(status);
						LOGGER.info(status ? "wget is blocked for destination hostname" : errorMessage);
					}
				}
			}
			status = (!statusArrayList.contains(false));
			if (status) {
				LOGGER.info("STEP 12: ACTUAL :wget is blocked for destination ip ");
			} else {
				LOGGER.error("STEP 12: ACTUAL : " + errorMessage);
			}
			LOGGER.info("******************************************************************");

			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			// flush and verify the ips blacklisted
			testStepNumber = "s13";
			errorMessage = null;
			status = false;
			LOGGER.info("******************************************************************");
			LOGGER.info("STEP 13 : DESCRIPTION : flush and verify the ips blacklisted");
			LOGGER.info("STEP 13 : ACTION : Execute the below Command: ipset flush ip_blacklist");
			LOGGER.info("STEP 13 : EXPECTED : The command should execute successfully");
			LOGGER.info("******************************************************************");
			response = tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_FLUSH_IPSET_LIST);
			status = CommonMethods.isNull(response);
			errorMessage = "Failed to flush ip blacklist for hash:ip";
			LOGGER.info(
					"STEP 13: ACTUAL :  " + (status ? "Successfully flushed ip blacklist for hash:ip" : errorMessage));

			LOGGER.info("******************************************************************");

			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			// verify the ipset list after flushing the ipset list
			testStepNumber = "s14";
			errorMessage = null;
			status = false;
			LOGGER.info("******************************************************************");
			LOGGER.info("STEP 14 : DESCRIPTION : verify the ipset list after flushing the ipset list");
			LOGGER.info("STEP 14 : ACTION : Execute the below Command:" + BroadBandCommandConstants.CMD_LIST_IPSET);
			LOGGER.info("STEP 14 : EXPECTED : The command should execute successfully ");
			LOGGER.info("******************************************************************");
			status = CommonMethods
					.isNotNull(tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_LIST_IPSET));
			errorMessage = "Failed to verify ipset list after flushing the ipset list";

			LOGGER.info("STEP 14: ACTUAL :  "
					+ (status ? "Succcessfully verified the ipset list after flushing the ipset list" : errorMessage));

			LOGGER.info("******************************************************************");

			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			// Verify the destination ip reachability using icmp
			testStepNumber = "s15";
			errorMessage = null;
			status = false;
			statusArrayList.clear();
			LOGGER.info("******************************************************************");
			LOGGER.info("STEP 15 : DESCRIPTION : Verify the destination ip reachability using icmp");
			LOGGER.info("STEP 15 : ACTION : Execute Command: ping -c 2 ipAddress");
			LOGGER.info("STEP 15  : EXPECTED : Ping should  reach to the destination ip.");
			LOGGER.info("******************************************************************");
			for (String ipAddress : phishIPAddress) {
				commandToExecute = CommonUtils.concatStringUsingStringBuffer(
						BroadBandCommandConstants.CMD_PING_COUNT_TWO, BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
						ipAddress);
				response = tapEnv.executeCommandOnOneIPClients(clientSettop, commandToExecute);
				status = CommonMethods.isNotNull(response)
						&& (!CommonUtils.isGivenStringAvailableInCommandOutput(response,
								BroadBandTraceConstants.PACKET_LOSS_HUNDRED_PERCENT));
				errorMessage = "ping has not reached the specified ip Address: " + ipAddress;
				LOGGER.info(status ? "ping has reached the ip Address:" + ipAddress : errorMessage + ipAddress);
				statusArrayList.add(status);
			}
			LOGGER.info("Status Array List:" + statusArrayList);
			LOGGER.info("Ping Status Array List:" + pingStatusArrayList);
			status = (statusArrayList.equals(pingStatusArrayList));
			if (status) {
				LOGGER.info("STEP 15: ACTUAL :ping has reached the ip Address successfully");
			} else {
				LOGGER.error("STEP 15: ACTUAL : " + errorMessage);
			}
			LOGGER.info("******************************************************************");

			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			// verify the destination ip reachability using UDP"
			testStepNumber = "s16";
			errorMessage = null;
			status = false;
			statusArrayList.clear();
			LOGGER.info("******************************************************************");
			LOGGER.info("STEP 16 : DESCRIPTION :verify the destination ip reachability using  UDP");
			LOGGER.info("STEP 16 : ACTION : Execute the below Command: dig url @ipaddress");
			LOGGER.info("STEP 16 : EXPECTED : dig should  reach the destination given");
			LOGGER.info("******************************************************************");
			for (counter = BroadBandTestConstants.CONSTANT_0; counter < phishDomainName.size(); counter++) {
				commandToExecute = CommonUtils.concatStringUsingStringBuffer(BroadBandCommandConstants.CMD_DIG,
						BroadBandTestConstants.SINGLE_SPACE_CHARACTER, phishDomainName.get(counter),
						BroadBandTestConstants.SINGLE_SPACE_CHARACTER, BroadBandTestConstants.SYMBOL_AT,
						phishIPAddress.get(counter));
				response = tapEnv.executeCommandOnOneIPClients(clientSettop, commandToExecute);
				status = CommonMethods.isNotNull(response) && (!CommonUtils
						.isGivenStringAvailableInCommandOutput(response, BroadBandTraceConstants.CONNECTION_TIMED_OUT));
				errorMessage = "dig command is blocked for the destination url";
				statusArrayList.add(status);
			}
			LOGGER.info("Status Array List:" + statusArrayList);
			LOGGER.info("Dig Status Array List:" + digStatusArrayList);
			status = (statusArrayList.equals(digStatusArrayList));
			if (status) {
				LOGGER.info("STEP 16: ACTUAL :dig command has reached the destination url successfully ");
			} else {
				LOGGER.error("STEP 16: ACTUAL : " + errorMessage);
			}
			LOGGER.info("******************************************************************");

			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			// verify the destination ip reachability using TCP
			testStepNumber = "s17";
			errorMessage = null;
			status = false;
			statusArrayList.clear();
			LOGGER.info("******************************************************************");
			LOGGER.info("STEP 17 : DESCRIPTION : verify the destination ip reachability using TCP");
			LOGGER.info("STEP 17 : ACTION : Execute Below Command: wget http:url");
			LOGGER.info("STEP 17 : EXPECTED : wget should  reach the destination given");
			LOGGER.info("******************************************************************");
			for (String domainName : phishDomainName) {
				commandToExecute = CommonUtils.concatStringUsingStringBuffer(BroadBandCommandConstants.CMD_WGET_HTTP,
						domainName);
				response = tapEnv.executeCommandOnOneIPClients(clientSettop, commandToExecute);
				if (CommonMethods.isNotNull(response)) {
					status = (CommonUtils.isGivenStringAvailableInCommandOutput(response,
							BroadBandTraceConstants.HTTP_SUCCESS_CODE)
							|| CommonUtils.isGivenStringAvailableInCommandOutput(response,
									BroadBandTraceConstants.HTTP_FOUND_302_MESSAGE));
					errorMessage = "wget command is blocked for the destination url";
					statusArrayList.add(status);
				}
			}
			LOGGER.info("Status Array List:" + statusArrayList);
			LOGGER.info("wget Status Array List:" + wgetStatusArrayList);
			status = (statusArrayList.equals(wgetStatusArrayList));
			if (status) {
				LOGGER.info("STEP 17: ACTUAL :wget command has reached the destination url successfully ");
			} else {
				LOGGER.error("STEP 17: ACTUAL : " + errorMessage);
			}
			LOGGER.info("******************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, testStepNumber, status, errorMessage,
					true);
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-SYSTEM-IPSET-1008");
	}
	
	/**
	 * Verify common DHCPv4 code functionality
	 * <ol>
	 * <li>Verify SwitchToUDHCPC is disabled by default</li>
	 * <li>Set custom maintenance window based on device date</li>
	 * <li>Set SwitchToUDHCPC parameter to true</li>
	 * <li>Verify log message when SwitchToUDHCPC enabled</li>
	 * <li>Verify cronjob scheduled for dhcpswitch script in custom maintenance
	 * window</li>
	 * <li>Poll and verify SwitchToUDHCPC is enabled within maintenance window</li>
	 * <li>Poll and wait for UDHCPC process to get startedC</li>
	 * <li>Verify IPv4 address is obtained for erouter0 after enabling
	 * SwitchToUDHCP</li>
	 * <li>Verify WebPA process is up after WAN restart</li>
	 * <li>Verify IPv4 address obtained by client within DHCP range</li>
	 * <li>Verify IPv4 internet connectivity in connected client</li>
	 * <li>Verify udhcpc process restart</li>
	 * </ol>
	 * 
	 * @author Ashwin sankara
	 * @refactor yamini.s
	 * 
	 * @param device {@link Dut}
	 */

	@Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = {
			BroadBandTestGroup.WIFI })
	@TestDetails(testUID = "TC-RDKB-DHCP-SWITCH-1001")
	public void testVerifyUDHCPCSwitchFunctionality(Dut device) {

		// Variable Declaration begins
		String testCaseId = "TC-RDKB-DHCP-SWITCH-001";
		// String to store step number
		String stepNum = "s1";
		// String to store error message
		String errorMessage = null;
		// String to store command response
		String response = null;
		// String to store custom maintenance window start time
		String windowStartTime = null;
		// String to store custom maintenance window end time
		String windowEndTime = null;
		// String to store cron schedule minute value
		String cronMinuteValue = null;
		// String to store cron schedule hour value
		String cronHourValue = null;
		// long value to store time offset parameter
		long timeOffset = BroadBandTestConstants.CONSTANT_0;
		// long value to store converted cron schedule to seconds
		long value = BroadBandTestConstants.CONSTANT_0;
		// long value to store start time of polling period
		long startTime = BroadBandTestConstants.CONSTANT_0;
		// boolean to store result of step
		boolean status = false;
		// Connected client device object
		Dut clientSettop = null;
		// Broadband result object to store util output
		BroadBandResultObject result = new BroadBandResultObject();
		// Variable Declation Ends

		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-DHCP-SWITCH-1001");
		LOGGER.info("TEST DESCRIPTION: Verify common DHCPv4 code functionality");

		LOGGER.info("TEST STEPS : ");
		LOGGER.info("1. Verify SwitchToUDHCPC is disabled by default");
		LOGGER.info("2. Set custom maintenance window based on device date");
		LOGGER.info("3. Set SwitchToUDHCPC parameter to true");
		LOGGER.info("4. Verify log message when SwitchToUDHCPC enabled");
		LOGGER.info("5. Verify cronjob scheduled for dhcpswitch script in custom maintenance window");
		LOGGER.info("6. Poll and verify SwitchToUDHCPC is enabled within maintenance window");
		LOGGER.info("7. Poll and wait for UDHCPC process to get started");
		LOGGER.info("8. Verify IPv4 address is obtained for erouter0 after enabling SwitchToUDHCPC");
		LOGGER.info("9. Verify WebPA process is up after WAN restart");
		LOGGER.info("10. Verify IPv4 address obtained by client within DHCP range");
		LOGGER.info("11. Verify IPv4 internet connectivity in connected client");
		LOGGER.info("12. Verify udhcpc process restart");

		LOGGER.info("#######################################################################################");

		try {

			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION : DESCRIPTION : Fetch WiFi capable connected client, connect to SSID");
			LOGGER.info(
					"PRE-CONDITION : ACTION : Find WiFi client in list of connected devices and connect to 2.4 or 5 Ghz SSID.");
			LOGGER.info("PRE-CONDITION : EXPECTED : Successfully connected Client to 2.4 or 5 Ghz SSID");
			errorMessage = "Unable to get a client connected to 2.4 or 5 ghz band wifi";

			clientSettop = BroadBandPreConditionUtils.executePreConditionToVerifyWiFiClientStatus(device, tapEnv,
					BroadBandTestConstants.BAND_2_4GHZ_OR_5GHZ, BroadBandTestConstants.CONSTANT_1);
			status = null != clientSettop;
			LOGGER.info("PRE-CONDITION : ACTUAL: "
					+ (status ? "Successfully connected Client to 2.4 or 5 Ghz SSID and found another client"
							: errorMessage));
			LOGGER.info("PRE-CONFIGURATIONS : FINAL STATUS -  " + result);
			LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");
			if (!status) {
				throw new TestException(BroadBandCommonUtils
						.concatStringUsingStringBuffer(BroadBandTestConstants.PRE_CONDITION_ERROR, errorMessage));
			}

			errorMessage = "Default value of SwitchToUDHCPC is not false";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 1: DESCRIPTION : Verify SwitchToUDHCPC is disabled by default");
			LOGGER.info(
					"STEP 1: ACTION : Execute webpa command to get value of Device.DeviceInfo.X_RDKCENTRAL-COM_SwitchToUDHCPC.Enable");
			LOGGER.info("STEP 1: EXPECTED : Default value of SwitchToUDHCPC is false");
			LOGGER.info("**********************************************************************************");

			response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.WEBPA_PARAM_SWITCH_TO_UDHCPC);
			status = CommonMethods.isNotNull(response) && response.equalsIgnoreCase(BroadBandTestConstants.FALSE);

			if (status) {
				LOGGER.info("STEP 1: ACTUAL : Default value of SwitchToUDHCPC is false");
			} else {
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			LOGGER.info("**********************************************************************************");

			stepNum = "s2";
			errorMessage = "Obtained null value of time offset from parameter";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 2: DESCRIPTION : Set custom maintenance window based on device date");
			LOGGER.info(
					"STEP 2: ACTION : Execute date commands to get time in seconds and set maintenance window parameters");
			LOGGER.info(
					"STEP 2: EXPECTED : Set custom maintenance window for 15 minutes from current time successfully");
			LOGGER.info("**********************************************************************************");

			response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.WEBPA_PARAM_TIME_OFFSET);
			if (CommonMethods.isNotNull(response)) {
				errorMessage = "Obtained null response for time of the day in seconds using date command";
				timeOffset = Long.parseLong(response);
				response = BroadBandCommonUtils.getTimeOfDayInSeconds(device, tapEnv);
				if (CommonMethods.isNotNull(response)) {
					windowStartTime = Long
							.toString(Long.parseLong(response) + timeOffset + BroadBandTestConstants.CONSTANT_60);
					errorMessage = "Failed to set maintenance window start time as " + windowStartTime;
					windowEndTime = Long.toString(Long.parseLong(windowStartTime)
							+ Long.parseLong(BroadBandTestConstants.CONSTANT_NINE_HUNDRED));
					if (BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
							BroadBandWebPaConstants.WEBPA_COMMAND_MAINTENANCE_WINDOW_START_TIME,
							BroadBandTestConstants.CONSTANT_0, windowStartTime)) {
						errorMessage = "Failed to set maintenance window end time as " + windowEndTime;
						status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
								BroadBandWebPaConstants.WEBPA_COMMAND_MAINTENANCE_WINDOW_END_TIME,
								BroadBandTestConstants.CONSTANT_0, windowEndTime);
					}
				}
			}

			if (status) {
				LOGGER.info(
						"STEP 2: ACTUAL : Set custom maintenance window for 15 minutes from current time successfully");
			} else {
				LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			LOGGER.info("**********************************************************************************");

			stepNum = "s3";
			errorMessage = "Failed to set value of SwitchToUDHCPC using webpa";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 3: DESCRIPTION : Set SwitchToUDHCPC parameter to true");
			LOGGER.info(
					"STEP 3: ACTION : Execute webpa command to set value of Device.DeviceInfo.X_RDKCENTRAL-COM_SwitchToUDHCPC.Enable to true");
			LOGGER.info("STEP 3: EXPECTED : WebPA set is success");
			LOGGER.info("**********************************************************************************");

			status = BroadBandWiFiUtils.setWebPaParams(device, BroadBandWebPaConstants.WEBPA_PARAM_SWITCH_TO_UDHCPC,
					BroadBandTestConstants.TRUE, BroadBandTestConstants.CONSTANT_3);

			if (status) {
				LOGGER.info("STEP 3: ACTUAL : WebPA set is success");
			} else {
				LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			LOGGER.info("**********************************************************************************");

			stepNum = "s4";
			errorMessage = "Failed to get dhcpclient_v4 log message when SwitchToUDHCPC is enabled";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 4: DESCRIPTION : Verify log message when SwitchToUDHCPC enabled");
			LOGGER.info("STEP 4: ACTION : Execute command:grep -i dhcpclient_v4 /rdklogs/logs/PAMlog.txt.0");
			LOGGER.info("STEP 4: EXPECTED : Log message is present when SwitchToUDHCPC is enabled");
			LOGGER.info("**********************************************************************************");

			status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
					BroadBandTraceConstants.LOG_MESSAGE_DHCPCLIENT_V4, BroadBandTestConstants.COMMAND_NTP_LOG_FILE,
					BroadBandTestConstants.TWO_MINUTE_IN_MILLIS, BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS));

			if (status) {
				LOGGER.info("STEP 4: ACTUAL : Log message is present when SwitchToUDHCPC is enabled");
			} else {
				LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s5";
			errorMessage = "Failed to find dhcpswitch script scheduled in crontab after enabling";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 5: DESCRIPTION : Verify cronjob scheduled for dhcpswitch script in custom maintenance window");
			LOGGER.info("STEP 5: ACTION : Execute grep -i \"dhcpswitch.sh\" /var/spool/cron/crontabs/root,"
					+ " Convert cron schedule to seconds and verify value is within custom maintenance window");
			LOGGER.info(
					"STEP 5: EXPECTED : Cronjob is scheduled with dhcpswitch script within custom maintenance window after enabling");
			LOGGER.info("**********************************************************************************");

			response = BroadBandCommonUtils.searchLogFiles(tapEnv, device, BroadBandTestConstants.DHCP_SWITCH_SCRIPT,
					BroadBandCommandConstants.ROOT_CRON_TAB);
			if (CommonMethods.isNotNull(response)
					&& CommonMethods.patternMatcher(response, BroadBandTestConstants.DHCP_SWITCH_SCRIPT)) {
				errorMessage = "Obtained null value of minute from cron schedule";
				cronMinuteValue = CommonMethods.patternFinder(response,
						BroadBandTestConstants.PATTERN_MATCHER_GET_NUMBER);
				if (CommonMethods.isNotNull(cronMinuteValue)) {
					errorMessage = "Obtained null value of hour from cron schedule";
					value = Long.parseLong(cronMinuteValue.trim()) * BroadBandTestConstants.CONSTANT_60;
					cronHourValue = CommonMethods
							.patternFinderToReturnAllMatchedString(response,
									BroadBandTestConstants.PATTERN_MATCHER_GET_NUMBER)
							.get(BroadBandTestConstants.CONSTANT_1);
					if (CommonMethods.isNotNull(cronHourValue)) {
						errorMessage = "Cronschedule for dhcpscript.sh is not within set custom maintenance window";
						value = value + (Long.parseLong(cronHourValue.trim()) * BroadBandTestConstants.CONSTANT_60
								* BroadBandTestConstants.CONSTANT_60);
						LOGGER.info("Cronschedule time in seconds: " + value);
						status = (value >= (Long.parseLong(windowStartTime) - timeOffset))
								&& (value <= (Long.parseLong(windowEndTime) - timeOffset));
					}
				}
			}

			if (status) {
				LOGGER.info(
						"STEP 5: ACTUAL : Cronjob is scheduled with dhcpswitch script within custom maintenance window after enabling");
			} else {
				LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			LOGGER.info("**********************************************************************************");

			stepNum = "s6";
			errorMessage = "SwitchToUDHCPC failed to set to true in maintenance window after enabling";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 6: DESCRIPTION : Poll and verify SwitchToUDHCPC is enabled within maintenance window");
			LOGGER.info("STEP 6: ACTION : Execute webpa command to get value of SwitchToUDHCPC parameter");
			LOGGER.info("STEP 6: EXPECTED : SwitchToUDHCPC set to true successfully");
			LOGGER.info("**********************************************************************************");

			startTime = System.currentTimeMillis();
			do {
				response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_SWITCH_TO_UDHCPC);
				status = CommonMethods.isNotNull(response) && response.equalsIgnoreCase(BroadBandTestConstants.TRUE);
			} while (System.currentTimeMillis() - startTime < BroadBandTestConstants.TWENTY_MINUTES_IN_MILLIS && !status
					&& BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS));

			if (status) {
				LOGGER.info("STEP 6: ACTUAL : SwitchToUDHCPC set to true successfully");
			} else {
				LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			LOGGER.info("**********************************************************************************");

			stepNum = "s7";
			errorMessage = "Unable to get pid for udhcpc after 5 minutes of switching";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 7: DESCRIPTION : Poll and wait for UDHCPC process to get started");
			LOGGER.info("STEP 7: ACTION : Execute command:pidof udhcpc");
			LOGGER.info("STEP 7: EXPECTED : Obtained pid for udhcpc after switching");
			LOGGER.info("**********************************************************************************");

			startTime = System.currentTimeMillis();
			do {
				status = CommonMethods.isNotNull(
						CommonMethods.getPidOfProcess(device, tapEnv, BroadBandTestConstants.PROCESS_NAME_UDHCPC));
			} while (System.currentTimeMillis() - startTime < BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS && !status
					&& BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));

			if (status) {
				LOGGER.info("STEP 7: ACTUAL : Obtained pid for udhcpc after switching");
			} else {
				LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			LOGGER.info("**********************************************************************************");

			stepNum = "s8";
			errorMessage = "Failed to obtain IPv4 address for erouter0 after enabling SwitchToUDHCPC";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 8: DESCRIPTION : Verify IPv4 address is obtained for erouter0 after enabling SwitchToUDHCPC");
			LOGGER.info("STEP 8: ACTION : Execute command:ifconfig erouter0");
			LOGGER.info("STEP 8: EXPECTED : Valid IPv4 address is obtained for erouter0  interface");
			LOGGER.info("**********************************************************************************");

			do {
				status = CommonMethods.isNotNull(BroadBandCommonUtils.getErouteripv4Address(device, tapEnv));
			} while (System.currentTimeMillis() - startTime < BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS && !status
					&& BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));

			if (status) {
				LOGGER.info("STEP 8: ACTUAL : Valid IPv4 address is obtained for erouter0  interface");
			} else {
				LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s9";
			errorMessage = "Failed to verify webpa process up after wan restart";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 9: DESCRIPTION : Verify WebPA process is up after WAN restart");
			LOGGER.info("STEP 9: ACTION : Execute webpa command to get serial number");
			LOGGER.info("STEP 9: EXPECTED : WebPA process is up after WAN restart");
			LOGGER.info("**********************************************************************************");

			status = BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true);

			if (status) {
				LOGGER.info("STEP 9: ACTUAL : WebPA process is up after WAN restart");
			} else {
				LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s10";
			errorMessage = "Failed to connect any WiFi client successfully";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 10: DESCRIPTION : Verify IPv4 address obtained by client within DHCP range");
			LOGGER.info(
					"STEP 10: ACTION : Connect WiFi or ethernet client to Gateway and execute command: ipconfig /all on windows client, ifconfig on linux client");
			LOGGER.info("STEP 10: EXPECTED : IPv4 address within DHCP range is obtained by client");
			LOGGER.info("**********************************************************************************");

			clientSettop = BroadBandConnectedClientUtils
					.get2GhzOr5GhzWiFiCapableClientDeviceAndConnectToCorrespondingSsid(device, tapEnv);
			if (clientSettop != null) {
				errorMessage = "Failed to obtain IPv4 address within DHCP range in connected client";
				status = BroadBandConnectedClientUtils.verifyIpv4AddressOFConnectedClientIsBetweenDhcpRange(tapEnv,
						device, clientSettop);
			}

			if (status) {
				LOGGER.info("STEP 10: ACTUAL : IPv4 address within DHCP range is obtained by client");
			} else {
				LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s11";
			errorMessage = "Failed to connect to https://www.instagram.com using IPv4 address from client";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 11: DESCRIPTION : Verify IPv4 internet connectivity in connected client");
			LOGGER.info(
					"STEP 11: ACTION : Execute command:curl --connect-timeout 20 --head -4 https://www.instagram.com");
			LOGGER.info("STEP 11: EXPECTED : Curl connection is success using IPv4");
			LOGGER.info("**********************************************************************************");

			result = BroadBandConnectedClientUtils.verifyInternetIsAccessibleInConnectedClientUsingCurl(tapEnv,
					clientSettop, BroadBandTestConstants.URL_WIKIPEDIA, BroadBandTestConstants.IP_VERSION4);
			status = result.isStatus();
			errorMessage = result.getErrorMessage();

			if (status) {
				LOGGER.info("STEP 11: ACTUAL : Curl connection is success using IPv4");
			} else {
				LOGGER.error("STEP 11: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s12";
			errorMessage = "Failed to restart Udhcpc after crashing by self heal";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 12: DESCRIPTION : Verify udhcpc process restart");
			LOGGER.info(
					"STEP 12: ACTION : Execute command: pidof udhcpc, kill -11 <pid>, Wait for self heal window and execute: pidof udhcpc");
			LOGGER.info("STEP 12: EXPECTED : Udhcpc process is restarted successfully");
			LOGGER.info("**********************************************************************************");

			status = BroadBandSystemUtils.killAndVerifyProcessRestarted(device, tapEnv,
					BroadBandTestConstants.PROCESS_NAME_UDHCPC);

			if (status) {
				LOGGER.info("STEP 12: ACTUAL : Udhcpc process is restarted successfully");
			} else {
				LOGGER.error("STEP 12: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					false);
		} finally {

			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			LOGGER.info("POST-CONDITION STEPS");
			LOGGER.info(
					"POST-CONDITION : DESCRIPTION : Disable SwitchToUDHCPC and revert maintenance window parameter values");
			LOGGER.info(
					"POST-CONDITION : ACTION : Reset values of Device.DeviceInfo.X_RDKCENTRAL-COM_SwitchToUDHCPC.Enable,"
							+ " Device.DeviceInfo.X_RDKCENTRAL-COM_MaintenanceWindow.FirmwareUpgradeStartTime and"
							+ " Device.DeviceInfo.X_RDKCENTRAL-COM_MaintenanceWindow.FirmwareUpgradeEndTime");
			LOGGER.info("POST-CONDITION : EXPECTED : Post condition executed successsfully");

			status = false;
			errorMessage = "Unable to reset value of FirmwareUpgradeStartTime to 0";
			if (BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_COMMAND_MAINTENANCE_WINDOW_START_TIME,
					BroadBandTestConstants.CONSTANT_0, BroadBandTestConstants.STRING_ZERO)) {
				errorMessage = "Unable to reset value of FirmwareUpgradeEndTime to 10800";
				if (BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_COMMAND_MAINTENANCE_WINDOW_END_TIME,
						BroadBandTestConstants.CONSTANT_0, BroadBandTestConstants.STRING_VALUE_10800)) {
					errorMessage = "Unable to reset value of SwitchToUDHCPC to false";
					status = BroadBandWebPaUtils.verifyWebPaValueAfterDuration(device, tapEnv,
							BroadBandWebPaConstants.WEBPA_PARAM_SWITCH_TO_UDHCPC, BroadBandTestConstants.CONSTANT_3,
							BroadBandTestConstants.FALSE, BroadBandTestConstants.THREE_MINUTE_IN_MILLIS);
				}
			}

			if (status) {
				LOGGER.info("POST-CONDITION : ACTUAL : Post condition executed successfully");
			} else {
				LOGGER.error("POST-CONDITION : ACTUAL : Post condition failed: " + errorMessage);
			}
			LOGGER.info("POST-CONFIGURATIONS : FINAL STATUS - " + status);
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-DHCP-SWITCH-1001");
	}

}
