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

package com.automatics.rdkb.tests.wifi;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.enums.ExecutionStatus;
import com.automatics.rdkb.TestGroup;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.constants.RDKBTestConstants.WiFiFrequencyBand;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;
import com.automatics.rdkb.utils.wifi.BroadBandWiFiUtils;
import com.automatics.rdkb.utils.wifi.connectedclients.BroadBandConnectedClientUtils;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadBandPreConditionUtils;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.ConnectedNattedClientsUtils;

public class BroadBandWifiSSIDPerformanceTest extends AutomaticsTestBase{
	
    /**
     * Integer to store Iteration value 20 for Wifi Performance test
     */
    public static final Integer WIFI_CNTY_PERFORMANCE_MAXIMUM_NUMBER_OF_ITERATION = 20;
    
    /** Integer to store Iteration value 10 for Wifi Performance test */
    public static final Integer WIFI_PERFORMANCE_MAXIMUM_NUMBER_OF_ITERATION = 10;
    
    /**
     * Successful Wi-Fi connectivity rate of 2.4GHz radio band for 10 times in Wireless Gateway
     * <ol>
     * <li>ITERATION STEP 1:Connect to 2.4 Ghz wifi SSID .</li>
     * <li>ITERATION STEP 2:Verify whether connection with gateway IP is successful</li>
     * <li>ITERATION STEP 3:Verify the ccspwifi process should not be affected</li>
     * <li>ITERATION STEP 4:Verify the hostapd process should not be affected</li>
     * <li>ITERATION STEP 5:Disconnect from 2.4Ghz SSID</li>
     * <li>Iterate the ITERATION STEP 1-STEP 5 for ten times</li>
     * <li>STEP 1:Calculate Success Rate of the successful connections</li>
     * <li>STEP 2.Calculate ccspwifi process validation success Rate</li>
     * <li>STEP 3.Calculate hostapd process validation success Rate</li>
     * <li>STEP 4:Verify the 2.4 GHz SSID using webpa</li>
     * @param device
     *            Dut instance
     * @author PRASANTH REDDY ANDENA
     * @refactor Athira
     * </ol>
     */
    @Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, groups = {
	    TestGroup.WEBPA, TestGroup.WIFI }, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-WIFI-CON-PERF-1000")
    public void wifiPerformanceTest24Ghz(Dut device) {
	boolean status = false;// String to store the test case status
	String testId = "TC-RDKB-WIFI-CON-PERF-100";// Test case id
	String testStep = "s1";// Test step number
	String errorMessage = null;// String to store the error message
	String response = null;// String to store response
	Integer successCount = 0;// Integer to store successcount
	Integer wifiProcessSuccessCnt = 0;// Integer to store ccp wifi process successcount
	Integer hostapdSuccessCnt = 0;// Integer to store hostapd process successcount
	String gatewayIp = null;// String to store ip address of gateway
	String command = null;// String to store command
	float successRate2Ghz = 0;// Float to store success rate of 2.4Ghz
	float hostapdSuccessRate2Ghz = 0;
	float wifiProcesssuccessRate2Ghz = 0;// Float to store ccp wifi process success rate of 2.4Ghz
	String ssidName = null;// String to store SSID name
	Integer iteration = 0;// Integer to store iteration value

	try {
	    LOGGER.info("#################### STARTING TEST CASE: TC-RDKB-WIFI-CON-PERF-1000 #####################");
	    LOGGER.info(
		    "TEST DESCRIPTION:Successful Wi-Fi connectivity rate of 2.4GHz radio band for 10 times in Wireless Gateway ");
	    LOGGER.info("TEST STEPS : ");
	    LOGGER.info("ITERATION STEP: 1.Connect to 2.4 Ghz wifi SSID ");
	    LOGGER.info("ITERATION STEP: 2.Verify whether connection with gateway IP is successful");
	    LOGGER.info("ITERATION STEP: 3.Verify the ccspwifi process should not be affected");
	    LOGGER.info("ITERATION STEP: 4.Verify the hostapd process should not be affected");
	    LOGGER.info("ITERATION STEP: 5.Disconnect from 2.4Ghz SSID ");
	    LOGGER.info("Iterate the 1-5 ITERATION STEP for ten times");
	    LOGGER.info("1.Calculate Success Rate of the successful connections");
	    LOGGER.info("2.Calculate ccspwifi process validation success Rate");
	    LOGGER.info("3.Calculate hostapd process validation success Rate");
	    LOGGER.info("4:Verify the 2.4 GHz SSID using webpa");
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("Successful Wi-Fi connectivity rate of 2.4GHz radio band with 10 times in Wireless Gateway");
	    LOGGER.info("##########################################################################");
	    gatewayIp = BroadBandCommonUtils.getIpAddressFromGivenInterface(device, tapEnv,
			BroadBandTestConstants.INTERFACE_NAME_BRLAN0, true);
	    for (int count = 0; count < WIFI_PERFORMANCE_MAXIMUM_NUMBER_OF_ITERATION; count++) {
		iteration = count + 1;
		/**
		 * ITERATION STEP: 1: Connect to 2.4Ghz WIFI SSID
		 */
		LOGGER.info("##########################################################################");
		LOGGER.info("ITERATION :" + iteration + " STEP 1 : Connect to 2.4Ghz SSID");
		errorMessage = "Unable to connect the client with 2.4GHz Wi-Fi Network";
		// Throws Test exception when conn client not obtained
		Dut connectedSetop = BroadBandConnectedClientUtils
			.get2GhzWiFiCapableClientDeviceAndConnectToAssociated2GhzSsid(device, tapEnv);
		LOGGER.info("ITERATION :" + iteration + " STEP 1 ACUTAL : Successfully Connected to 2.4Ghz SSID");
		command = BroadBandWiFiUtils.commandToPingBasedOnTypeOfOs(connectedSetop, gatewayIp);
		LOGGER.info("##########################################################################");
		/**
		 * ITERATION STEP 2:Verify connection with gateway IP from connected client
		 */
		LOGGER.info("###########################################################################");
		LOGGER.info("ITERATION :" + iteration
			+ " STEP 2 : Verify connection with gateway ip from connected client");
		errorMessage = "Unable to establish a wifi connection with the router";
		response = tapEnv.executeCommandOnOneIPClients(connectedSetop, command);
		if ((CommonUtils.patternSearchFromTargetString(response,
			BroadBandTestConstants.STRING_LINUX_PING_RESPONSE))
			|| ((CommonUtils.patternSearchFromTargetString(response,
				BroadBandTestConstants.STRING_WINDOWS_PING_RESPONSE)))
				&& CommonMethods.isNotNull(response)) {
		    LOGGER.info("ITERATION :" + iteration
			    + " STEP 2 : ACUTAL : Successfully Verified connection with gateway ip");
		    successCount++;
		} else {
		    LOGGER.error("ITERATION :" + iteration + " STEP 2 ACUTAL : " + errorMessage);
		}
		LOGGER.info("##########################################################################");
		/**
		 * ITERATION STEP: 3: Verify the ccspwifi process should not be affected.
		 */
		status = false;
		LOGGER.info("##########################################################################");
		LOGGER.info("ITERATION :" + iteration + " STEP 3 : Verify the ccspwifi process should not be affected");
		errorMessage = "ccspwifi process went down after consecutive wifi connections.";
		if (BroadBandCommonUtils.verifyProcessId(device, tapEnv, false,
			BroadBandCommandConstants.CMD_GET_WIFI_PROCESS,
			BroadBandTestConstants.PATTERN_MATCHER_TO_GET_WIFI_PROCESS_PID)) {
		    wifiProcessSuccessCnt++;
		    LOGGER.info("ITERATION :" + iteration
			    + " STEP 3 ACUTAL : Ccspwifi process is not affected, As expected.");
		} else {
		    LOGGER.error("ITERATION :" + iteration + " STEP 3 ACUTAL : " + errorMessage);
		}
		LOGGER.info("##########################################################################");
		/**
		 * ITERATION STEP: 4: Verify the hostapd process should not be affected.
		 */
		status = false;
		LOGGER.info("##########################################################################");
		LOGGER.info("ITERATION :" + iteration + " STEP 4 :Verify the  hostapd  process should not be affected");
		errorMessage = "hostapd process went down after consecutive wifi connections.";
		if (CommonMethods.isAtomSyncAvailable(device, tapEnv)) {
		    if (BroadBandCommonUtils.verifyProcessId(device, tapEnv, true,
			    BroadBandCommandConstants.CMD_GET_HOSTAPD_PROCESS,
			    BroadBandTestConstants.PATTERN_MATCHER_TO_GET_WIFI_PROCESS_PID)) {
			hostapdSuccessCnt++;
			LOGGER.info("ITERATION :" + iteration
				+ " STEP 4 ACUTAL : hostapd process is not affected, As expected.");
		    } else {
			LOGGER.error("ITERATION :" + iteration + " STEP 4 ACUTAL : " + errorMessage);
		    }
		} else {
		    errorMessage = "Skipping hostapd process validation, As it is applicable only to Atom based devices.";
		    LOGGER.info("ITERATION :" + iteration + " STEP 4 ACUTAL : " + errorMessage);
		}
		LOGGER.info("##########################################################################");
		/**
		 * ITERATION STEP 5:Disconnect from 5Ghz SSID
		 */
		status = false;
		LOGGER.info("###########################################################################");
		LOGGER.info("ITERATION :" + iteration + " STEP 5 : Disconnect from 2.4Ghz SSID");
		if (connectedSetop != null) {
		    errorMessage = "Unable to disconnect connected client from wifi network";
		    ssidName = BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(device, tapEnv,
			    WiFiFrequencyBand.WIFI_BAND_2_GHZ);
		    if (ConnectedNattedClientsUtils.disconnectSSID(connectedSetop, tapEnv, ssidName)) {
			LOGGER.info("ITERATION :" + iteration
				+ " STEP 5 ACUTAL : Connected client disconnected from Wi-Fi network successfully");
		    } else {
			LOGGER.error("ITERATION :" + iteration + " STEP 5 ACUTAL : " + errorMessage);
		    }
		}
		LOGGER.info("Waiting for 90 seconds");
		tapEnv.waitTill(BroadBandTestConstants.NINTY_SECOND_IN_MILLIS);
		LOGGER.info("##########################################################################");
	    }
	    /**
	     * STEP 1:Calculate Success Rate of the successful connections.
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 1 : DESCRIPTION : Calculate successful Wi-Fi connectivity rate for 2.4Ghz SSID for ten iterations");
	    LOGGER.info("STEP 1 : ACTION : Calculate Wi-Fi connectivity rate");
	    LOGGER.info("STEP 1 : EXPECTED : Successful Wi-Fi connectivity rate should be 100%");
	    LOGGER.info("**********************************************************************************");

	    successRate2Ghz = successCount * 100 / WIFI_PERFORMANCE_MAXIMUM_NUMBER_OF_ITERATION;
	    errorMessage = "Number of successful connections made is less than number of wifi connectivity attempts. Acutal number of successful connections : "
		    + successCount;
	    if (successCount == WIFI_PERFORMANCE_MAXIMUM_NUMBER_OF_ITERATION) {
		status = true;
		LOGGER.info("STEP 1:ACTUAL:Wi-Fi connectivity rate for 2.4Ghz for " + successCount
			+ " Successful iterations out of 10 iterations is :" + successRate2Ghz);
	    } else {
		LOGGER.error("STEP 1:ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("Wi-Fi connectivity rate for 2.4Ghz is :" + successRate2Ghz + "%");
	    tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, false);
	    LOGGER.info("##########################################################################");

	    /**
	     * STEP 2:Calculate ccspwifi process validation success Rate for ten iterations
	     */
		LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 2 : DESCRIPTION : Calculate ccspwifi process validation success Rate for ten iterations");
	    LOGGER.info("STEP 2 : ACTION : Calculate ccspwifi process validation success Rate");
	    LOGGER.info("STEP 2 : EXPECTED : Successful ccspwifi process validation success Rate should be 100%");
	    LOGGER.info("**********************************************************************************");
		 
	    status = false;
	    testStep = "s2";

	    wifiProcesssuccessRate2Ghz = wifiProcessSuccessCnt * 100 / WIFI_PERFORMANCE_MAXIMUM_NUMBER_OF_ITERATION;
	    errorMessage = "Number of successful ccspwifi process validation attempts made is less than number of ccspwifi process validation attempts. Acutal number of successful ccspwifi process validation : "
		    + wifiProcessSuccessCnt;
	    if (wifiProcessSuccessCnt == WIFI_PERFORMANCE_MAXIMUM_NUMBER_OF_ITERATION) {
		status = true;
		LOGGER.info("STEP 2:ACTUAL:Wi-Fi connectivity rate for 2.4Ghz is : " + wifiProcessSuccessCnt
			+ " Successful iterations out of 10 iterations is : " + wifiProcesssuccessRate2Ghz);
	    } else {
		LOGGER.error("STEP 2:ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("Ccspwifi process validation success Rate for 2.4Ghz is :" + wifiProcesssuccessRate2Ghz + "%");
	    tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, false);
	    LOGGER.info("##########################################################################");

	    /**
	     * STEP 3:Calculate hostapd process validation success Rate for ten iterations
	     */
		LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 3 : DESCRIPTION : Calculate hostapd process validation success Rate for ten iterations");
	    LOGGER.info("STEP 3 : ACTION : Calculate hostapd process validation success Rate");
	    LOGGER.info("STEP 3 : EXPECTED : Successful hostapd process validation success Rate should be 100%");
	    LOGGER.info("**********************************************************************************"); 
		 
	    status = false;
	    testStep = "s3";
	    if (CommonMethods.isAtomSyncAvailable(device, tapEnv)) {
		hostapdSuccessRate2Ghz = hostapdSuccessCnt * 100 / WIFI_PERFORMANCE_MAXIMUM_NUMBER_OF_ITERATION;
		errorMessage = "Number of successful hostapd process validation attempts made is less than number of hostapd process validation attempts. Acutal number of successful hostapd process validation : "
			+ hostapdSuccessCnt;
		if (hostapdSuccessCnt == WIFI_PERFORMANCE_MAXIMUM_NUMBER_OF_ITERATION) {
		    status = true;
		    LOGGER.info("STEP 3:ACTUAL: Hostapd success rate for 5Ghz is : " + hostapdSuccessCnt
			    + " Successful iterations out of 10 iterations is : " + hostapdSuccessRate2Ghz);
		} else {
		    LOGGER.error("STEP 3: ACTUAL: " + errorMessage);
		}
		LOGGER.info("hostapd process validation success Rate for 5Ghz is :" + hostapdSuccessRate2Ghz + "%");
		tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, false);
	    } else {
		errorMessage = "Skipping hostapd process validation, As it is applicable only to Atom based devices.";
		LOGGER.info("STEP 3: ACTUAL: "+ errorMessage);
		tapEnv.updateExecutionForAllStatus(device, testId, testStep, ExecutionStatus.NOT_APPLICABLE,
			errorMessage, false);
	    }
	    LOGGER.info("##########################################################################");
	    /**
	     * STEP 4: Verify the 2.4 GHz SSID using webpa.
	     */
		LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 4 : DESCRIPTION : GET THE 2.4 GHZ PRIVATE WIFI SSID NAME");
	    LOGGER.info("STEP 4 : ACTION : GET THE 2.4 GHZ PRIVATE WIFI SSID NAME USING WEBPA :"
	    			+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_NAME);
	    LOGGER.info("STEP 4 : EXPECTED : Successful hostapd process validation success Rate should be 100%");
	    LOGGER.info("**********************************************************************************"); 
	    
	    status = false;
	    int stepNumber = 4;
	    testStep = "s" + stepNumber;
	    errorMessage = "UNABLE TO GET THE 2.4 GHZ PRIVATE WIFI SSID NAME USING WEBPA";

	    LOGGER.info("OBTAINED SSID NAME FOR " + WiFiFrequencyBand.WIFI_BAND_2_GHZ + " IS : " + ssidName);
	    status = CommonMethods.isNotNull(ssidName);
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL: SUCCESSFULLY RETRIEVED THE 2.4GHZ PRIVATE WIFI SSID NAME USING WEBPA");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("##########################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, true);
	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error("Failure in execution of Wi-Fi connectivity rate of 2.4GHz radio band \n" + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, testStep, status, errorMessage, true);
	} 
	LOGGER.info("COMPLETED TEST CASE: TC-RDKB-WIFI-CON-PERF-1000");
	LOGGER.info("#######################################################################################");
    }
    
    /**
     * Successful Wi-Fi connectivity rate of 5Ghz radio band for 10 times in Wireless Gateway
     * <ol>
     * <li>ITERATION STEP 1:Connect to 5Ghz wifi SSID</li>
     * <li>ITERATION STEP 2:Verify whether connection with gateway IP is successful</li>
     * <li>ITERATION STEP 3:Verify the ccspwifi process should not be affected</li>
     * <li>ITERATION STEP 4:Verify the hostapd process should not be affected</li>
     * <li>ITERATION STEP 5:Disconnect from 5Ghz SSID</li>
     * <li>Iterate the ITERATION STEP 1-STEP 5 for ten times</li>
     * <li>STEP 1:Calculate Success Rate of 5GHZ SSID successful connection</li>
     * <li>STEP 2.Calculate ccspwifi process validation success Rate</li>
     * <li>STEP 3.Calculate hostapd process validation success Rate</li>
     * </ol>
	 * @param device
     *            Dut instance
     * @author PRASANTH REDDY ANDENA
     * @refactor Athira
	 
     */
    @Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, groups = {
	    TestGroup.WEBPA, TestGroup.WIFI }, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-WIFI-CON-PERF-1001")
    public void wifiPerformanceTest5Ghz(Dut device) {
	boolean status = false;// String to store the test case status
	String testId = "TC-RDKB-WIFI-CON-PERF-101";// Test case id
	String testStep = "s1";// Test step number
	String errorMessage = null;// String to store the error message
	String response = null;// String to store response
	Integer successCount = 0;// Integer to store successcount
	Integer wifiProcessSuccessCnt = 0;// Integer to store ccp wifi process successcount
	Integer hostapdSuccessCnt = 0;// Integer to store hostapd process successcount
	String gatewayIp = null;// String to store ip address of gateway
	String command = null;// String to store command
	float successRate5Ghz = 0;// Float to store success rate of 5Ghz
	float wifiProcessSuccessRate5Ghz = 0;// Float to store ccp wifi process success rate of 5Ghz
	float hostapdSuccessRate5Ghz = 0;
	String ssidName = null;// String to store SSID name
	Integer iteration = 0;// Integer to store iteration value

	try {
	    LOGGER.info("#################### STARTING TEST CASE: TC-RDKB-WIFI-CON-PERF-1001 #####################");
	    LOGGER.info(
		    "TEST DESCRIPTION: Successful Wi-Fi connectivity rate of 5Ghz radio band for 10 times in Wireless Gateway");
	    LOGGER.info("TEST STEPS : ");
	    LOGGER.info("ITERATION STEP: 1.Connect to 5Ghz wifi SSID");
	    LOGGER.info("ITERATION STEP: 2.Verify whether connection with gateway IP is successful");
	    LOGGER.info("ITERATION STEP: 3.Verify the ccspwifi process should not be affected");
	    LOGGER.info("ITERATION STEP: 4.Verify the hostapd process should not be affected");
	    LOGGER.info("ITERATION STEP: 5.Disconnect from 5Ghz SSID ");
	    LOGGER.info("Iterate the 1-5 ITERATION STEP for ten times");
	    LOGGER.info("1.Calculate Success Rate of the successful connections");
	    LOGGER.info("2.Calculate ccspwifi process validation success Rate");
	    LOGGER.info("3.Calculate hostapd process validation success Rate");
	    LOGGER.info("4:Verify the 5 GHz SSID using webpa");
	    LOGGER.info("#####################################################################################");

	    LOGGER.info("##########################################################################");
	    LOGGER.info("Successful Wi-Fi connectivity rate of 5GHz radio band with 10 times in Wireless Gateway");
	    LOGGER.info("##########################################################################");
	    gatewayIp = BroadBandCommonUtils.getIpAddressFromGivenInterface(device, tapEnv,
			BroadBandTestConstants.INTERFACE_NAME_BRLAN0, true);
	    for (int count = 0; count < WIFI_PERFORMANCE_MAXIMUM_NUMBER_OF_ITERATION; count++) {
		/**
		 * ITERATION STEP 1: Connect to 5Ghz WIFI SSID
		 */
		iteration = count + 1;
		LOGGER.info("##########################################################################");
		LOGGER.info("ITERATION :" + iteration + " STEP 1: Connect to 5Ghz SSID ");
		// Throws Test exception when conn client not obtained
		Dut connectedSetop = BroadBandConnectedClientUtils
			.get5GhzWiFiCapableClientDeviceAndConnectToAssociated5GhzSsid(device, tapEnv);
		LOGGER.info("ITERATION :" + iteration + " STEP 1 : ACUTAL : Successfully Connected to 5Ghz SSID ");
		command = BroadBandWiFiUtils.commandToPingBasedOnTypeOfOs(connectedSetop, gatewayIp);
		LOGGER.info("##########################################################################");
		/**
		 * ITERATION STEP 2:Verify connection with gateway IP from connected client
		 */
		LOGGER.info("###########################################################################");
		LOGGER.info("ITERATION :" + iteration
			+ " STEP 2 : Verify connection with gateway ip from connected client");
		errorMessage = "The ping on connected client failed";
		response = tapEnv.executeCommandOnOneIPClients(connectedSetop, command);
		if ((CommonUtils.patternSearchFromTargetString(response,
			BroadBandTestConstants.STRING_LINUX_PING_RESPONSE))
			|| ((CommonUtils.patternSearchFromTargetString(response,
				BroadBandTestConstants.STRING_WINDOWS_PING_RESPONSE)))
				&& CommonMethods.isNotNull(response)) {
		    LOGGER.info("ITERATION :" + iteration
			    + " STEP 2 : ACUTAL : Successfully Verified connection with gateway ip");
		    successCount++;
		} else {
		    LOGGER.error("ITERATION :" + iteration + " STEP 2 ACUTAL : " + errorMessage);
		}
		LOGGER.info("##########################################################################");
		/**
		 * ITERATION STEP: 3: Verify the ccspwifi process should not be affected.
		 */
		status = false;
		LOGGER.info("##########################################################################");
		LOGGER.info("ITERATION :" + iteration + " STEP 3 : Verify the ccspwifi process should not be affected");
		errorMessage = "ccspwifi process went down after consecutive wifi connections.";
		if (BroadBandCommonUtils.verifyProcessId(device, tapEnv, false,
			BroadBandCommandConstants.CMD_GET_WIFI_PROCESS,
			BroadBandTestConstants.PATTERN_MATCHER_TO_GET_WIFI_PROCESS_PID)) {
		    wifiProcessSuccessCnt++;
		    LOGGER.info("ITERATION :" + iteration
			    + " STEP 3 ACUTAL : Ccspwifi process is not affected, As expected.");
		} else {
		    LOGGER.error("ITERATION :" + iteration + " STEP 3 ACUTAL : " + errorMessage);
		}
		LOGGER.info("##########################################################################");
		/**
		 * ITERATION STEP: 4: Verify the hostapd process should not be affected.
		 */
		status = false;
		LOGGER.info("##########################################################################");
		LOGGER.info("ITERATION :" + iteration + " STEP 4 :Verify the  hostapd  process should not be affected");
		errorMessage = "hostapd process went down after consecutive wifi connections.";
		if (CommonMethods.isAtomSyncAvailable(device, tapEnv)) {
		    if (BroadBandCommonUtils.verifyProcessId(device, tapEnv, true,
			    BroadBandCommandConstants.CMD_GET_HOSTAPD_PROCESS,
			    BroadBandTestConstants.PATTERN_MATCHER_TO_GET_WIFI_PROCESS_PID)) {
			hostapdSuccessCnt++;
			LOGGER.info("ITERATION :" + iteration
				+ " STEP 3 ACUTAL : hostapd process is not affected, As expected.");
		    } else {
			LOGGER.error("ITERATION :" + iteration + " STEP 4 ACUTAL : " + errorMessage);
		    }
		} else {
		    errorMessage = "Skipping hostapd process validation, As it is applicable only for atom based devices.";
		    LOGGER.info("ITERATION :" + iteration + " STEP 4 ACUTAL : " + errorMessage);
		}
		LOGGER.info("##########################################################################");
		/**
		 * ITERATION STEP 5:Disconnect from 5Ghz SSID
		 */
		status = false;
		LOGGER.info("###########################################################################");
		LOGGER.info("ITERATION :" + iteration + " STEP 5 : Disconnect from 5Ghz SSID");
		if (connectedSetop != null) {
		    errorMessage = "Unable to disconnect connected client from wifi network";
		    ssidName = BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(device, tapEnv,
			    WiFiFrequencyBand.WIFI_BAND_5_GHZ);
		    if (ConnectedNattedClientsUtils.disconnectSSID(connectedSetop, tapEnv, ssidName)) {
			LOGGER.info("ITERATION :" + iteration
				+ " STEP 5 ACUTAL : Connected client disconnected from Wi-Fi network successfully");
		    } else {
			LOGGER.error("ITERATION :" + iteration + " STEP 5 ACUTAL : " + errorMessage);
		    }
		}
		LOGGER.info("Waiting for 90 seconds");
		tapEnv.waitTill(BroadBandTestConstants.NINTY_SECOND_IN_MILLIS);
		LOGGER.info("##########################################################################");
	    }
	    /**
	     * STEP 1:Calculate Success Rate of the successful connections.
	     */
	    status = false;
	    testStep = "s1";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 1 : DESCRIPTION : Calculate successful Wi-Fi connectivity rate for 5Ghz SSID for ten iterations");
	    LOGGER.info("STEP 1 : ACTION : Calculate Wi-Fi connectivity rate");
	    LOGGER.info("STEP 1 : EXPECTED : Successful Wi-Fi connectivity rate should be 100%");
	    LOGGER.info("**********************************************************************************");

	    successRate5Ghz = successCount * 100 / WIFI_PERFORMANCE_MAXIMUM_NUMBER_OF_ITERATION;
	    errorMessage = "Number of successful connections made is less than number of wifi connectivity attempts. Acutal number of successful connections : "
		    + successCount;
	    if (successCount == WIFI_PERFORMANCE_MAXIMUM_NUMBER_OF_ITERATION) {
		status = true;
		LOGGER.info("STEP 1:ACTUAL:Wi-Fi connectivity rate for 5Ghz for " + successCount
			+ " Successful iterations out of 10 iterations is :" + successRate5Ghz);
	    } else {
		LOGGER.error("STEP 1:ACTUAL:" + errorMessage);
	    }
	    LOGGER.info("Wi-Fi connectivity rate for 5Ghz is :" + successRate5Ghz + "%");
	    tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, false);
	    LOGGER.info("##########################################################################");

	    /**
	     * STEP 2:Calculate ccspwifi process validation success Rate for ten iterations
	     */
	    status = false;
	    testStep = "s2";
		LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 2 : DESCRIPTION : Calculate ccspwifi process validation success Rate for ten iterations");
	    LOGGER.info("STEP 2 : ACTION : Calculate ccspwifi process validation success Rate");
	    LOGGER.info("STEP 2 : EXPECTED : Successful ccspwifi process validation success Rate should be 100%");
	    LOGGER.info("**********************************************************************************");

	    wifiProcessSuccessRate5Ghz = wifiProcessSuccessCnt * 100 / WIFI_PERFORMANCE_MAXIMUM_NUMBER_OF_ITERATION;
	    errorMessage = "Number of successful ccspwifi process validation attempts made is less than number of ccspwifi process validation attempts. Acutal number of successful ccspwifi process validation : "
		    + wifiProcessSuccessCnt;
	    errorMessage = "Success rate is zero/less than number of  :ACTUAL RESPONSE: " + wifiProcessSuccessRate5Ghz;
	    if (wifiProcessSuccessCnt == WIFI_PERFORMANCE_MAXIMUM_NUMBER_OF_ITERATION) {
		status = true;
		LOGGER.info("STEP 2:ACTUAL:Wi-Fi connectivity rate for 5Ghz is : " + wifiProcessSuccessCnt
			+ " Successful iterations out of 10 iterations is : " + wifiProcessSuccessRate5Ghz);
	    } else {
		LOGGER.error("STEP 2:ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("Ccspwifi process validation success Rate for 5Ghz is :" + wifiProcessSuccessRate5Ghz + "%");
	    tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, false);
	    LOGGER.info("##########################################################################");

	    /**
	     * STEP 3: Calculate hostapd process validation success Rate for ten iterations
	     */
	    status = false;
	    testStep = "s3";
		LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 3 : DESCRIPTION : Calculate hostapd process validation success Rate for ten iterations");
	    LOGGER.info("STEP 3 : ACTION : Calculate hostapd process validation success Rate");
	    LOGGER.info("STEP 3 : EXPECTED : Successful hostapd process validation success Rate should be 100%");
	    LOGGER.info("**********************************************************************************");
	    
	    if (CommonMethods.isAtomSyncAvailable(device, tapEnv)) {
		hostapdSuccessRate5Ghz = hostapdSuccessCnt * 100 / WIFI_PERFORMANCE_MAXIMUM_NUMBER_OF_ITERATION;
		errorMessage = "Number of successful hostapd process validation attempts made is less than number of hostapd process validation attempts. Acutal number of successful hostapd process validation : "
			+ hostapdSuccessCnt;
		if (hostapdSuccessCnt == WIFI_PERFORMANCE_MAXIMUM_NUMBER_OF_ITERATION) {
		    status = true;
		    LOGGER.info("STEP 3:ACTUAL: Hostapd success rate for 5Ghz is : " + hostapdSuccessCnt
			    + " Successful iterations out of 10 iterations is : " + hostapdSuccessRate5Ghz);
		} else {
		    LOGGER.error("STEP 3: ACTUAL: " + errorMessage);
		}
		LOGGER.info("hostapd process validation success Rate for 5Ghz is :" + hostapdSuccessRate5Ghz + "%");
		tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, false);
	    } else {
		errorMessage = "Skipping hostapd process validation, As it is applicable only to Atom based devices.";
		LOGGER.info("STEP 3: ACTUAL: " + errorMessage);
		tapEnv.updateExecutionForAllStatus(device, testId, testStep, ExecutionStatus.NOT_APPLICABLE,
			errorMessage, false);
	    }
	    LOGGER.info("##########################################################################");
	    /**
	     * STEP 4: Verify the 5 GHz SSID using webpa.
	     */
	    status = false;
	    int stepNumber = 4;
	    testStep = "s" + stepNumber;
		LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 4 : DESCRIPTION : GET THE 5 GHZ PRIVATE WIFI SSID NAME");
	    LOGGER.info("STEP 4 : ACTION : GET THE 5 GHZ PRIVATE WIFI SSID NAME USING WEBPA :"
	    	+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_NAME);
	    LOGGER.info("STEP 4 : EXPECTED : Successful ccspwifi process validation success Rate should be 100%");
	    LOGGER.info("**********************************************************************************");
	    
	    errorMessage = "UNABLE TO GET THE 5 GHZ PRIVATE WIFI SSID NAME USING WEBPA";

	    LOGGER.info("OBTAINED SSID NAME FOR " + WiFiFrequencyBand.WIFI_BAND_5_GHZ + " IS : " + ssidName);
	    status = CommonMethods.isNotNull(ssidName);
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL: SUCCESSFULLY RETRIEVED THE 5 GHZ PRIVATE WIFI SSID NAME USING WEBPA");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("##########################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, true);

	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error("Failure in execution of Wi-Fi connectivity rate of 5Ghz radio band \n" + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, testStep, status, errorMessage, true);
	}
	LOGGER.info("COMPLETED TEST CASE: TC-RDKB-WIFI-CON-PERF-1001");
	LOGGER.info("#######################################################################################");
    }
    
    /**
     * Average mean time for Wi-Fi connection establishment of 2.4GHz radio band for 20 times
     * <ol>
     * <li>PRE-CONDITION 1 : DESCRIPTION : OBTAIN A 2.4GHZ WIFI CLIENT ASSOSIATED WITH THE GATEWAY</li>
     * <li>PRE-CONDITION 2 : DESCRIPTION :VERIFY THE CORRECT IPV4 ADDRESS FOR WIFI CLIENT</li>
     * <li>PRE-CONDITION 3 : DESCRIPTION :VERIFY THE CORRECT IPV6 ADDRESS FOR WIFI CLIENT</li>
     * <li>PRE-CONDITION 4 : DESCRIPTION : VERIFY THE INTERNET CONNECTIVITY IN THE CONNECTED WIFI CLIENT USING IPV4
     * INTERFACE</li>
     * <li>PRE-CONDITION 5 : DESCRIPTION : VERIFY THE INTERNET CONNECTIVITY IN THE CONNECTED WIFI CLIENT USING IPV6
     * INTERFACE</li>
     * <li>ITERATION STEP 1:Mean time for Wi-Fi connection establishment of 2.4GHz radio band .</li>
     * <li>Repeat the ITERATION STEP 1 for twenty times totally 20 steps</li>
     * <li>STEP 1:Calculate Success rate for Wi-Fi connection establishment of 2.4GHz radio band for 20 iterations</li>
     * <li>STEP 2:Calculate Mean time for Wi-Fi connection establishment of 2.4GHz radio band for 20 iterations.</li>
     * </ol>
     * @param device
     *            Dut instance
     * @author PRASANTH REDDY ANDENA
     * @refactor Athira
     */
    @Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, groups = {
	    TestGroup.WEBPA, TestGroup.WIFI }, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-WIFI-CON-PERF-1002")
    public void wifiAvgPerformanceTest24Ghz(Dut device) {

	String testId = "TC-RDKB-WIFI-CON-PERF-102";// Test case id

	LOGGER.info("#################### STARTING TEST CASE: TC-RDKB-WIFI-CON-PERF-1002 #####################");

	LOGGER.info(
		"TEST DESCRIPTION: Average mean time for Wi-Fi connection establishment of 2.4GHz radio band for 20 times");
	LOGGER.info("PRE-CONDITION 1 : DESCRIPTION : OBTAIN A 2.4GHZ WIFI CLIENT ASSOSIATED WITH THE GATEWAY");
	LOGGER.info("PRE-CONDITION 2 : DESCRIPTION :VERIFY THE CORRECT IPV4 ADDRESS FOR WIFI CLIENT");
	LOGGER.info("PRE-CONDITION 3 : DESCRIPTION :VERIFY THE CORRECT IPV6 ADDRESS FOR WIFI CLIENT");

	LOGGER.info(
		"PRE-CONDITION 4 : DESCRIPTION : VERIFY THE INTERNET CONNECTIVITY IN THE CONNECTED WIFI CLIENT USING IPV4 INTERFACE");
	LOGGER.info(
		"PRE-CONDITION 5 : DESCRIPTION : VERIFY THE INTERNET CONNECTIVITY IN THE CONNECTED WIFI CLIENT USING IPV6 INTERFACE");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info("ITERATION STEP 1:Mean time for Wi-Fi connection establishment of 2.4GHz radio band ");
	LOGGER.info("Repeat the ITERATION STEP 1 for twenty times totally 20 steps");
	LOGGER.info(
		"1.Calculate Success rate for Wi-Fi connection establishment of 2.4GHz radio band for 20 iterations");
	LOGGER.info("2.Calculate Mean time for Wi-Fi connection establishment of 2.4GHz radio band for 20 iterations.");
	LOGGER.info("#####################################################################################");
	LOGGER.info("Average mean time for Wi-Fi connection establishment of 2.4GHz radio band for 20 times");
	LOGGER.info("##########################################################################");
	/**
	 * Helper method for Performance test case 2.4Ghz SSID
	 */
	performanceTestOnWifi(device, testId, BroadBandTestConstants.BAND_2_4GHZ);
	
	LOGGER.info("#################### ENDING TEST CASE: TC-RDKB-WIFI-CON-PERF-1002 #####################");

    }

    /**
     * Average mean time for Wi-Fi connection establishment of 5GHz radio band for 20 times
     * <ol>
     * <li>PRE-CONDITION 1 : DESCRIPTION : OBTAIN A 2.4GHZ WIFI CLIENT ASSOSIATED WITH THE GATEWAY</li>
     * <li>PRE-CONDITION 2 : DESCRIPTION :VERIFY THE CORRECT IPV4 ADDRESS FOR WIFI CLIENT</li>
     * <li>PRE-CONDITION 3 : DESCRIPTION :VERIFY THE CORRECT IPV6 ADDRESS FOR WIFI CLIENT</li>
     * <li>PRE-CONDITION 4 : DESCRIPTION : VERIFY THE INTERNET CONNECTIVITY IN THE CONNECTED WIFI CLIENT USING IPV4
     * INTERFACE</li>
     * <li>PRE-CONDITION 5 : DESCRIPTION : VERIFY THE INTERNET CONNECTIVITY IN THE CONNECTED WIFI CLIENT USING IPV6
     * INTERFACE</li>
     * <li>ITERATION STEP 1:Mean time for Wi-Fi connection establishment of 5GHz radio band .</li>
     * <li>Repeat the ITERATION STEP 1 for twenty times totally 20 steps</li>
     * <li>STEP 1:Calculate Success rate for Wi-Fi connection establishment of 5GHz radio band for 20 iterations</li>
     * <li>STEP 2:Calculate Mean time for Wi-Fi connection establishment of 5GHz radio band for 20 iterations.</li>
     * @param device
     *            Dut instance
     * @author PRASANTH REDDY ANDENA
     * @refactor Athira
     * </ol>
     */
	
    @Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, groups = {
    	    TestGroup.WEBPA, TestGroup.WIFI }, dataProviderClass = AutomaticsTapApi.class)
        @TestDetails(testUID = "TC-RDKB-WIFI-CON-PERF-1003")
        public void wifiAvgPerformanceTest5Ghz(Dut device) {
    	
    	String testId = "TC-RDKB-WIFI-CON-PERF-103";

    	LOGGER.info("#################### STARTING TEST CASE: TC-RDKB-WIFI-CON-PERF-1003 #####################");
    	LOGGER.info(
    		"TEST DESCRIPTION: Average mean time for Wi-Fi connection establishment of 5GHz radio band for 20 times");
    	LOGGER.info("PRE-CONDITION 1 : DESCRIPTION : OBTAIN A 5GHZ WIFI CLIENT ASSOSIATED WITH THE GATEWAY");
    	LOGGER.info("PRE-CONDITION 2 : DESCRIPTION :VERIFY THE CORRECT IPV4 ADDRESS FOR WIFI CLIENT");
    	LOGGER.info("PRE-CONDITION 3 : DESCRIPTION :VERIFY THE CORRECT IPV6 ADDRESS FOR WIFI CLIENT");
    	LOGGER.info(
    		"PRE-CONDITION 4 : DESCRIPTION : VERIFY THE INTERNET CONNECTIVITY IN THE CONNECTED WIFI CLIENT USING IPV4 INTERFACE");
    	LOGGER.info(
    		"PRE-CONDITION 5 : DESCRIPTION : VERIFY THE INTERNET CONNECTIVITY IN THE CONNECTED WIFI CLIENT USING IPV6 INTERFACE");

    	LOGGER.info("TEST STEPS : ");
    	LOGGER.info("ITERATION STEP 1.Mean time for Wi-Fi connection establishment of 5GHz radio band ");
    	LOGGER.info("Repeat the ITERATION STEP 1 step for twenty times totally 20 steps");
    	LOGGER.info("1.Calculate Success rate for Wi-Fi connection establishment of 5GHz radio band for 20 iterations");
    	LOGGER.info("2.Calculate Mean time for Wi-Fi connection establishment of 5GHz radio band for 20 iterations.");
    	LOGGER.info("#####################################################################################");

    	LOGGER.info("##########################################################################");
    	LOGGER.info("Average mean time for Wi-Fi connection establishment of 5GHz radio band for 20 times ");
    	LOGGER.info("##########################################################################");
    	/**
    	 * Helper method for Performance test case 5Ghz SSID
    	 */
    	performanceTestOnWifi(device, testId, BroadBandTestConstants.BAND_5GHZ);
    	
    	LOGGER.info("#################### STARTING TEST CASE: TC-RDKB-WIFI-CON-PERF-1003 #####################");
    }
    
    /**
     * HELPER METHOD FOR WIFI PERFORMANCE TEST 2.4GHZ/5GHZ
     * 
     * @param device
     *            Dut instance
     * @param testId
     *            String Test case
     * @param wifiBand
     *            String Wifi Frequency band
     * @refactor Athira
     */
    public void performanceTestOnWifi(Dut device, String testId, String wifiBand) {
	boolean status = false;// boolean to store the test case status
	String testStep = null;// Test step number
	String errorMessage = null;// String to store the error message
	Integer successCount = 0;// Integer to store successcount
	long totalConnTimeValue = 0;// long to store average value
	boolean blockedStatus = false;
	float successRate = 0;// float to store success rate of 2.4Ghz
	long connectedTime = 0;// Long to store connection time

	try {
	    /**
	     * Precondition to retrieve Connected client associated with 2.4Ghz/5Ghz wifi band
	     */		
		Dut connectedClient = null;
		
		//Precondition deprecated
		connectedClient = BroadBandPreConditionUtils.executePreConditionToVerifyWiFiClientStatus(device,
			    tapEnv, wifiBand, 1);

	    /**
	     * ITERATION STEP 1:Mean time for Wi-Fi connection establishment of 2.4GHz/5Ghz radio band
	     */
	    LOGGER.info("ITERATION STEP :Mean time for Wi-Fi connection establishment of " + wifiBand + " radio band");
	    LOGGER.info(
		    "EXPECTED:Wifi connection establishment should be successful and time difference is calculated ");
	    for (int iteration = 1; iteration <= WIFI_CNTY_PERFORMANCE_MAXIMUM_NUMBER_OF_ITERATION; iteration++) {
		LOGGER.info("##########################################################################");
		LOGGER.info(
			"ITERATION :" + iteration + " ITERATION STEP :Mean time for Wi-Fi connection establishment of "
				+ wifiBand + " radio band");
		connectedTime = BroadBandConnectedClientUtils.connectToGivenWiFiCapableClientForPerfTest(device, tapEnv,
			connectedClient, wifiBand);
		LOGGER.info("connectedTime in iteration " +iteration +" is "+connectedTime);
		if (connectedTime != 0) {
		    successCount++;
		    LOGGER.info("successCount in iteration " +iteration +" is "+successCount);
		    totalConnTimeValue += connectedTime;
		    LOGGER.info("totalConnTimeValue after iteration " +iteration +" is "+totalConnTimeValue);
		} else {
		    LOGGER.error("Failure in time difference calculation reason might be connectivity issue");
		}
		LOGGER.info("Waiting for 90 seconds");
		tapEnv.waitTill(BroadBandTestConstants.NINTY_SECOND_IN_MILLIS);
		LOGGER.info("##########################################################################");
	    }
	    LOGGER.info("successCount after 20 iteration is "+successCount);
	    LOGGER.info("totalConnTimeValue after 20 iteration is "+totalConnTimeValue);
	    /**
	     * STEP 1:Calculate Success rate for Wi-Fi connection establishment of 2.4GHz/5Ghz radio band
	     */

	    testStep = "s1";
	    status = false;
	    blockedStatus = false;
	    errorMessage = "Success Count for Wi-Fi connection establishment of " + wifiBand + " radio band is "
		    + successCount + "/" + WIFI_CNTY_PERFORMANCE_MAXIMUM_NUMBER_OF_ITERATION;
	    LOGGER.info("##########################################################################");
	    LOGGER.info("STEP 1: DESCRIPTION : Calculate Success rate for Wi-Fi connection establishment of " + wifiBand
		    + " radio band for 20 iterations");
	    LOGGER.info("STEP 1 : EXPECTED: Success rate should be calculated successfully with 20/20 rate");
	    if (successCount == WIFI_CNTY_PERFORMANCE_MAXIMUM_NUMBER_OF_ITERATION) {
		status = true;
		LOGGER.info("STEP 1:ACTUAL: Wi-Fi Connection with " + wifiBand
			+ " is Successfully established for 20 iterations");
	    } else if (successCount == 0) {
		errorMessage = "Wifi connection with " + wifiBand
			+ " is not established ,Successful iterations are Zero";
		LOGGER.error("STEP 1:ACTUAL:" + errorMessage);
		blockedStatus = true;
	    } else {
		LOGGER.error("STEP 1:ACTUAL:" + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, blockedStatus);
	    LOGGER.info("##########################################################################");
	    /**
	     * STEP 2:Calculate Mean time for Wi-Fi connection establishment of 2.4GHz radio band
	     */
	    testStep = "s2";
	    status = false;
	    errorMessage = "Unable to calculate Mean time for wifi successful connections";
	    LOGGER.info("##########################################################################");
	    LOGGER.info("STEP 2: DESCRIPTION : Calculate Mean time for Wi-Fi connection establishment of " + wifiBand
		    + " radio band for successful iterations");
	    LOGGER.info("STEP 2: EXPECTED: Mean time should be calculated successfully for successful iteration");
	    successRate = (totalConnTimeValue / successCount);
	    status = (successRate > 0);
	    if (status) {
		LOGGER.info("STEP 2:ACTUAL: Mean time for Wi-Fi connection establishment of " + wifiBand
			+ " radio band for " + successCount + " Successful iterations is : " + successRate + "msec");
	    } else {
		LOGGER.error("STEP 2:ACTUAL:" + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, false);

	    LOGGER.info("##########################################################################");
	    LOGGER.info("Mean time for Wi-Fi connection establishment of " + wifiBand + " :" + successRate
		    + "msecs for " + successCount + "/" + WIFI_CNTY_PERFORMANCE_MAXIMUM_NUMBER_OF_ITERATION);
	    LOGGER.info("##########################################################################");
	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error("Failure in execution in Average mean time for Wi-Fi connection establishment of " + wifiBand
		    + " radio band \n" + errorMessage);
	}
    }
}
