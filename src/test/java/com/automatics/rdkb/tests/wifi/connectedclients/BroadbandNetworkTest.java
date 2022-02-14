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

import org.openqa.selenium.By;
import org.openqa.selenium.WebElement;
import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.ConnectedNattedClientsUtils;
import com.automatics.rdkb.utils.wifi.connectedclients.BroadBandConnectedClientUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;

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
}
