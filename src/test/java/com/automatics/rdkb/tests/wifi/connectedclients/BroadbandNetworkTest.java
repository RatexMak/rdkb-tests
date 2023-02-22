/**
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
package com.automatics.rdkb.tests.wifi.connectedclients;

import java.util.HashMap;
import java.util.Map;

import org.openqa.selenium.By;
import org.openqa.selenium.WebElement;
import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Device;
import com.automatics.device.Dut;
import com.automatics.enums.ExecutionStatus;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.BroadBandResultObject;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.constants.BroadBandConnectedClientTestConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandTraceConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.constants.RDKBTestConstants.WiFiFrequencyBand;
import com.automatics.rdkb.utils.BroadBandPreConditionUtils;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.ConnectedNattedClientsUtils;
import com.automatics.rdkb.utils.DeviceModeHandler;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.rdkb.utils.wifi.connectedclients.BroadBandConnectedClientUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;
import com.automatics.rdkb.utils.BroadBandCommonUtils;

/***
 * Class to hold the test cases related to the network connectivity tests in connected client devices
 * 
 * 
 */
public class BroadbandNetworkTest extends AutomaticsTestBase {
    
    

    /**
     * Ping to an ipv6 address from an Ethernet connected device.
     * <ol>
     * <li>Get an Ethernet connected client.</li>
     * <li>Ping to an IPV6 address</li>
     * <li>Execute the nslookup command for google.com in ethernet client connected</li>
     * </ol>
     * 
     * @author Ramees KV
     * @refactor Govardhan
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, groups = BroadBandTestGroup.SYSTEM)
    @TestDetails(testUID = "TC-RDKB-NW-CHECK-1001")
    public void pingToIpv6Address(Dut device) {

	// Variable Declaration begins
	String testCaseId = null;
	String stepNum = null;
	String errorMessage = null;
	boolean status = false;
	Dut clientSettop = null;
	// Variable Declaration Ends

	testCaseId = "TC-RDKB-NW-CHECK-101";
	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-NW-CHECK-1001");
	LOGGER.info("TEST DESCRIPTION: Ping to an ipv6 address from an Ethernet connected device.");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Get an Ethernet connected client.");
	LOGGER.info("2. Ping to an IPV6 address");
	LOGGER.info("3. Execute the nslookup command for google.com in ethernet client connected");
	LOGGER.info("#######################################################################################");
	try {
	    stepNum = "s1";
	    errorMessage = "Failed to get an Ethernet connected client";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION : Get an Ethernet connected client.");
	    LOGGER.info("STEP 1: ACTION : Get a device of capability Ethernet from connected device list");
	    LOGGER.info("STEP 1: EXPECTED : Device should be connected to Ethernet");
	    LOGGER.info("**********************************************************************************");
	    try {
		clientSettop = BroadBandConnectedClientUtils.getEthernetConnectedClient(tapEnv, device);
		status = (null != clientSettop);
	    } catch (TestException exception) {
		errorMessage = "Exception occured while verifying the ethernet connected devices : "
			+ exception.getMessage();
		LOGGER.error(errorMessage);
	    }
	    if (status) {
		LOGGER.info("STEP 1: ACTUAL: Obtained Ethernet connected client device ");
	    } else {
		LOGGER.error("STEP 1: ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "s2";
	    errorMessage = "Failed to get the response from the ping command";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 2: DESCRIPTION : Ping to an IPV6 address");
	    LOGGER.info(
		    "STEP 2: ACTION : Execute the below command   Example : ping 2a03:2880:f003:0c07:face:b00c:0000:0002");
	    LOGGER.info(
		    "STEP 2: EXPECTED : Ping shoulg be reachable to the IP and it should not have 100% packet loss   ");
	    LOGGER.info("**********************************************************************************");
	    try {
		status = ConnectedNattedClientsUtils.verifyPingConnectionForIpv4AndIpv6(clientSettop, tapEnv,
			BroadBandTestConstants.FACEBOOK_IPV6_ADDRESS, BroadBandTestConstants.IP_VERSION6);
	    } catch (TestException exception) {
		errorMessage = "Exception occured while verifying the Ping command : " + exception.getMessage();
		LOGGER.error(errorMessage);
	    }
	    if (status) {
		LOGGER.info("STEP 2: ACTUAL : Able to ping to an IPV6 address ");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "s3";
	    errorMessage = "Failed to get the response from nslookup command";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 3: DESCRIPTION : Execute the nslookup command for google.com in ethernet client connected");
	    LOGGER.info("STEP 3: ACTION : Execute the below command   Example : nslookup www.google.com");
	    LOGGER.info("STEP 3: EXPECTED : nslookup should return the proper IP address   ");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandConnectedClientUtils.verifyNsLookUpOnEthernetCnctdClient(tapEnv, clientSettop,
		    BroadBandTestConstants.NSLOOKUP_FOR_GOOGLE);
	    if (status) {
		LOGGER.info("STEP 3: ACTUAL : Got the proper response from the nslookup ");
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    true);
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-NW-CHECK-1001");
    }
    

	/**
	 * Verify DNS Address persist to the Wi-Fi & Ethernet client after reboot and
	 * ethernet client gets IP within acceptable time of 5 mins
	 * <ol>
	 * <li>PRE-CONDITION 1 : Verify ethernet client is connected with Gateway</li>
	 * <li>PRE-CONDITION 2 : Verify the wifi Connected client having private
	 * 2.4GHz/5GHz wifi Capability</li>
	 * <li>STEP 1. Verify the WebPA GET command to retrive Primary, Secondary DNS
	 * server IPv4, IPv6 address can be executed successfully</li>
	 * <li>STEP 2. Retrive and Verify the primary and secondary DNS server IPV4,
	 * IPv6 address assigned to the Wi-Fi client connected and cross verify with
	 * WEBPA Value retrieved in step 1.</li>
	 * <li>STEP 3. Reboot the device and check whether device is accessible after
	 * reboot</li>
	 * <li>STEP 4. Verify the IPv4 Address is retrieved from the client connected
	 * with Ethernet within the acceptable time of 5 minutes</li>
	 * <li>STEP 5. Reconnect to the Connected client having wifi Capability</li>
	 * <li>STEP 6. Retrieve and verify the primary and secondary DNS server IPV4,
	 * IPV6 address assigned to the wifi client connected and Cross verify with
	 * WEBPA Value retrieved in step 1(Persistance)</li>
	 * <li>STEP 7. Retrieve and verify the Connection-specific DNS suffix name from
	 * wifi client</li>
	 * </ol>
	 * 
	 * @param device {@link Dut}
	 * 
	 * @author Vignesh
	 * @refactor Athira
	 */

	@Test(enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = BroadBandTestGroup.WIFI)
	@TestDetails(testUID = "TC-RDKB-IPV4-CHECK-5001")
	public void testToVerifyDNSUpdateAndIpCheckAfterGatewayReboot(Dut device) {

		// Variable Declaration begins
		String testCaseId = "TC-RDKB-IPV4-CHECK-501";
		String errorMessage = null;
		boolean status = false;
		Dut deviceConnectedWithEthernet = null;
		Dut wifiConnectedClient = null;
		String response = null;
		String commandForDNSSuffix = null;
		int stepNumber = BroadBandTestConstants.CONSTANT_1;
		String stepNum = "S" + stepNumber;
		Map<String, String> webpaResponseMap = null;
		long startTime = 0L;
		long timeTaken = 0L;
		BroadBandResultObject broadBandResultObject = null;
		// Variable Declaration Ends

		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-IPV4-CHECK-5001");
		LOGGER.info(
				"TEST DESCRIPTION: Verify DNS update to the Wi-Fi & Ethernet client after reboot and ethernet client gets IP within acceptable time of 5 mins");
		LOGGER.info("PRE-CONDITION STEPS");
		LOGGER.info("PRE-CONDITION 1 : Verify ethernet client is connected with Gateway");
		LOGGER.info("PRE-CONDITION 2 : Connect to the Connected client having  private 2.4GHz/5GHz wifi Capability");
		LOGGER.info("TEST STEPS : ");
		LOGGER.info(
				"1. Verify the WebPA GET command to retrive Primary, Secondary DNS server IPv4, IPv6 address can be executed successfully.");
		LOGGER.info(
				"2. Retrive and Verify  the primary and secondary DNS server IPV4, IPv6 address assigned to the Wi-Fi client connected and cross verify with WEBPA Value retrieved in step 1.");
		LOGGER.info("3. Reboot the device and check whether device is accessible after reboot");
		LOGGER.info(
				"4. Verify the IPv4 Address is retrieved from the client connected with Ethernet within the acceptable time of 5 minutes");
		LOGGER.info("5. Reconnect to the Connected client having wifi Capability.");
		LOGGER.info(
				"6. Retrieve and verify the primary and secondary DNS server IPV4, IPV6 address assigned to the wifi client connected and Cross verify verify with WEBPA Value retrieved in step 1(Persistance).");
		LOGGER.info("7. Retrieve and verify the Connection-specific DNS suffix name from wifi client");
		LOGGER.info("#######################################################################################");
		try {

			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS");
			deviceConnectedWithEthernet = BroadBandPreConditionUtils.executePreConditionToVerifyLanClientStatus(device,
					tapEnv, BroadBandTestConstants.CONSTANT_1);

			wifiConnectedClient = BroadBandPreConditionUtils.executePreConditionToVerifyWiFiClientStatus(device, tapEnv,
					BroadBandTestConstants.BAND_2_4GHZ_OR_5GHZ, BroadBandTestConstants.CONSTANT_2);

			LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");
			// Step 1
			errorMessage = "Unable to Retrieve the Primary, Secondary DNS server IPv4, IPv6 address";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : Verify the WebPA GET command to retrive  Primary, Secondary DNS server IPv4, IPv6 address can be executed successfully.");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : Execute the command to retrive  Primary, Secondary DNS server IPv4, IPv6 address ");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : The retrived Primary DNS server IPv4, IPv6 address should not be null/empty.");
			LOGGER.info("**********************************************************************************");
			 webpaResponseMap = new HashMap<String, String>();
				if (!DeviceModeHandler.isRPIDevice(device)) {
					webpaResponseMap = tapEnv.executeMultipleWebPaGetCommands(device,
							BroadBandWebPaConstants.WEBPA_PARAMETER_ARRAY_TO_GET_DNS_IP_ADDRESS);
				} else {
					webpaResponseMap = tapEnv.executeMultipleWebPaGetCommands(device,
							BroadBandWebPaConstants.WEBPA_PARAM_PRIMARY_DNS_RPI);
				}
				status = (!webpaResponseMap.isEmpty() && (null != webpaResponseMap)
						&& BroadBandWebPaUtils.getAndVerifyMapValueIsNotNullOrEmpty(webpaResponseMap));
				if (status) {
					LOGGER.info("STEP " + stepNumber
							+ ": ACTUAL : DNS primary, secondary IPv4 ,IPv6 address are retrieved using webpa and verified successfully.");
				} else {
					LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			// Step 2 : Retrieves and validates DNS server ip Wi-Fi Connected client and
			// cross validate with webpa respose
			stepNumber++;
			verifyPrimaryAndSecondaryDnsIpRetrievedFromConnectedClient(device, tapEnv, testCaseId, stepNumber,
					wifiConnectedClient, webpaResponseMap);

			// Step 3
			stepNumber++;
			stepNum = "S" + stepNumber;
			errorMessage = "Device is not accessible after successful reboot of the gateway device.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : Reboot the device and check whether device comes up properly");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : Reboot the device using reboot command and wait for STB Accessible after successful reboot");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : STB should be reboot successfully and should be accessible after successful reboot.");
			LOGGER.info("**********************************************************************************");
			status = CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv);
			if (status) {
				startTime = System.currentTimeMillis();
				LOGGER.info(
						"STEP " + stepNumber + ": ACTUAL : Device is accessible after successful reboot as expected.");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			// Step 4
			stepNumber++;
		    stepNum = "S" + stepNumber;
		    errorMessage = "Failed to get Local IPv4 Address to the client within the acceptable time of 5 minutes";
		    status = false;
		    if (!DeviceModeHandler.isRPIDevice(device)) {
				LOGGER.info("**********************************************************************************");
				LOGGER.info("STEP " + stepNumber
						+ ": DESCRIPTION : Verify the IPv4 Address is retrieved  from the client connected with Ethernet within the acceptable time 5min");
				LOGGER.info("STEP " + stepNumber
						+ ": ACTION : Get the device IPv4 address using below commandLinux : ifconfig eth0 |grep -i \"inet addr:\"Windows: ipconfig |grep -A 10 \"Ethernet LAN adapter Wi-Fi\" |grep -i \"IPv4 Address\"");
				LOGGER.info("STEP " + stepNumber
						+ ": EXPECTED : Local IPv4 Address assigned to the client should be retrieved successfully within the acceptable time 5min");
				LOGGER.info("**********************************************************************************");
				do {
					broadBandResultObject = BroadBandConnectedClientUtils
							.verifyConnectedClientIpv4AddressInDhcpAfterRenew(device, tapEnv,
									deviceConnectedWithEthernet);
					timeTaken = System.currentTimeMillis() - startTime;
					LOGGER.info("TIME TAKEN IN MILLI SECONDS FOR CLIENT TO OBTAIN IPV4 AFTER GATEWAY REBOOT IS: "
							+ timeTaken);
					timeTaken = (timeTaken / BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
					status = (broadBandResultObject.isStatus() && (timeTaken <= BroadBandTestConstants.CONSTANT_5));
					errorMessage = broadBandResultObject.getErrorMessage();
				} while (!status
						&& (System.currentTimeMillis() - startTime) < BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS
						&& BroadBandCommonUtils.hasWaitForDuration(tapEnv,
								BroadBandTestConstants.TEN_SECOND_IN_MILLIS));
				if (status) {
					LOGGER.info(
							"TIME TAKEN IN MINUTES FOR CLIENT TO OBTAIN IPV4 AFTER GATEWAY REBOOT IS: " + timeTaken);
					LOGGER.info("STEP " + stepNumber
							+ ": ACTUAL : Connected Client Has Got IPV4 Within 5 Minutes After Successful Device Reboot.");
				} else {
					LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
				}

				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
			} else {
				LOGGER.info("skipping teststep due to device setup dependency ...");
				tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNum, ExecutionStatus.NOT_APPLICABLE,
						errorMessage, false);
			}
		    LOGGER.info("**********************************************************************************");
		    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			// Step 5
			stepNumber++;
			stepNum = "S" + stepNumber;
			errorMessage = "Unable to connect with private Wi-Fi Connected client.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : Reconnect to the Connected client having  private 2.4GHz/5GHz wifi Capability.");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : connect to the client having private wifi capablilty as 2.4/5 ghz/Dual band");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : Device should be able to connect with the connected client having 2.4GHZ/5GHz  wifi Capability");
			LOGGER.info("**********************************************************************************");
			BroadBandResultObject broadBandResultObjectWifi = BroadBandConnectedClientUtils
					.connectGivenConnectedClientToWifi(device, tapEnv, wifiConnectedClient,
							WiFiFrequencyBand.WIFI_BAND_2_GHZ);
			errorMessage += broadBandResultObjectWifi.getErrorMessage();
			if (!broadBandResultObjectWifi.isStatus()) {
				broadBandResultObjectWifi = BroadBandConnectedClientUtils.connectGivenConnectedClientToWifi(device,
						tapEnv, wifiConnectedClient, WiFiFrequencyBand.WIFI_BAND_5_GHZ);
			}
			errorMessage += broadBandResultObjectWifi.getErrorMessage();
			status = broadBandResultObjectWifi.isStatus();
			if (status) {
				LOGGER.info("STEP " + stepNumber + ": ACTUAL : Wifi Client connection to the device is successful ");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			// Step 6 : Retrieves and validates DNS server ip of wifi Connected client and
			// cross validate with webpa
			// response obtained before reboot
			stepNumber++;
			verifyPrimaryAndSecondaryDnsIpRetrievedFromConnectedClient(device, tapEnv, testCaseId, stepNumber,
					wifiConnectedClient, webpaResponseMap);
			// Step 7
			stepNumber++;
			stepNum = "S" + stepNumber;
			errorMessage = " DNS Domain Name showing as Utopia.net";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : Retrieve and verify the Connection-specific DNS suffix name ");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : Execute the command:For Windows:ipconfig | grep -i  Connection-specific DNS suffix , allFor Linux:nmcli dev show | grep DOMAIN");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : Retriving the Connection-specific DNS suffix name which should not be Utopia.net");
			LOGGER.info("**********************************************************************************");
			try {
				commandForDNSSuffix = ((Device) wifiConnectedClient).getOsType()
						.equalsIgnoreCase(BroadBandConnectedClientTestConstants.OS_WINDOWS)
								? BroadBandConnectedClientTestConstants.WINDOWS_COMMAND_TO_GET_DNS_SUFFIX
								: BroadBandConnectedClientTestConstants.CMD_LINUX_TO_RETRIEVE_DNS_SUFFIX;
				response = tapEnv.executeCommandOnOneIPClients(wifiConnectedClient, commandForDNSSuffix);
				status = CommonMethods.isNotNull(response)
						& !response.contains(BroadBandTraceConstants.DNS_Suffix_Name_Hijack);
			} catch (Exception e) {
				LOGGER.error(
						"Exception occured while chekcing the connction specific DNS Suffix name from Connected Client "
								+ e.getMessage());
			}
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ ": ACTUAL : SUCCESSFULLY VERIFIED THE DNS NAME AND RESPONSE IS EXPECTED");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					false);
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-IPV4-CHECK-5001");

	}
    
    /**
     * Test Steps to validate primary , seconday DNS IP from UI, webpa & Connected client
     * 
     * @param device
     *            {@link Dut}
     * @param tapEnv
     *            instance of {@link AutomaticsTapApi}
     * @param testCaseId
     *            Test Case ID
     * @param stepNumber
     *            Pre condition number
     * @param connectedClient
     *            Connected Client Object (Ethernet/Wifi)
     * @param webpaResponseMap
     *            To hold the WEBPA response in Map
     */
    public void verifyPrimaryAndSecondaryDnsIpRetrievedFromConnectedClient(Dut device, AutomaticsTapApi tapEnv,
	    String testCaseId, int stepNumber, Dut connectedClient, Map<String, String> webpaResponseMap) {
	String errorMessage = null;
	String stepNum = null;
	boolean status = false;
	String command = null;
	String response = null;
	try {
	    stepNum = "S" + stepNumber;
	    errorMessage = "Unable to validate the Primary,Secondary DNS server IPv4,IPv6 address";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Retrive and Verify  the primary and secondary DNS server IPV4, IPv6 address assigned to the client connected and cross verify with WEBPA value obtained in step 1.");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute the command:For Windows:ipconfig /allFor Linux:nmcli dev show | grep DNS                                                                                                                                                                                                                           ");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED : Retriving ,verifying primary and secondary DNS server IPV4, IPv6 address  assigned to the connected client should be successful and same as webpa value obtained in step 1.");
	    LOGGER.info("**********************************************************************************");
		command = ((Device) connectedClient).getOsType()
				.equalsIgnoreCase(BroadBandConnectedClientTestConstants.OS_WINDOWS)
						? BroadBandConnectedClientTestConstants.WINDOWS_COMMAND_TO_GET_IP_ADDRESS
						: BroadBandConnectedClientTestConstants.CMD_LINUX_TO_RETRIEVE_DNS_SERVER_IPS;
		LOGGER.info("command to execute on step " + stepNumber + " is " + command);
		response = tapEnv.executeCommandOnOneIPClients(connectedClient, command);
		LOGGER.info("response on step " + stepNumber + " is " + response);
		if (CommonMethods.isNotNull(response)) {
			if (!DeviceModeHandler.isRPIDevice(device)) {
				for (String key : BroadBandWebPaConstants.WEBPA_PARAMETER_ARRAY_TO_GET_DNS_IP_ADDRESS) {
					LOGGER.info("Key on step " + stepNumber + " is " + key);
					LOGGER.info(
							"webpaResponseMap.get(key) on step " + stepNumber + " is " + webpaResponseMap.get(key));
					status = CommonUtils.patternSearchFromTargetString(response, webpaResponseMap.get(key));
					LOGGER.info("status on step " + stepNumber + " is " + status);
					if (!status) {
						break;
					}
				}
			} else {
				for (String key : BroadBandWebPaConstants.WEBPA_PARAM_PRIMARY_DNS_RPI) {
					LOGGER.info("Key on step " + stepNumber + " is " + key);
					LOGGER.info(
							"webpaResponseMap.get(key) on step " + stepNumber + " is " + webpaResponseMap.get(key));
					status = CommonUtils.patternSearchFromTargetString(response, webpaResponseMap.get(key));
					LOGGER.info("status on step " + stepNumber + " is " + status);
					if (!status) {
						break;
					}
				}
			}
		}
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ ": ACTUAL : DNS primary, secondary IPv4 ,IPv6 address are retrieved from connected client and verified successfully with the WEBPA value obtained in Step 1");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	} catch (Exception e) {
	    LOGGER.error("Exception occured while Gettign the DNS address from GUI, WEBPA : " + e.getMessage());
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    false);
	}
    }
}
