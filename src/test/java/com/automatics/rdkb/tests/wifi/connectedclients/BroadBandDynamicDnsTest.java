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

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadBandPostConditionUtils;
import com.automatics.rdkb.utils.BroadBandPreConditionUtils;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;

public class BroadBandDynamicDnsTest extends AutomaticsTestBase {

	/** Constant holds the test step number with S **/
	private static String stepNum = "";

	/** Constant holds the test step number **/
	private static int stepNumber = 0;

	/** Constant holds the Error Message **/
	private static String errorMessage = null;

	/** Constant holds the test step status **/
	private static boolean status = false;

	/**
	 * Test Case : Verifying the alternate DNS nslookup functionality for 2.4GHz
	 * <p>
	 * STEPS:
	 * </p>
	 * <ol>
	 * <li>Test Description</li>
	 * <li>PRE-CONDITION 1 : Connect the client setup to 2.4 GHz SSID and verify
	 * connection status</li>
	 * <li>PRE-CONDITION 2 : Verify the correct IPv4 address for client connected
	 * with 2.4 GHz SSID</li>
	 * <li>PRE-CONDITION 3 : Verify the correct IPv6 address for client connected
	 * with 2.4 GHz SSID</li>
	 * <li>PRE-CONDITION 4 : Verify the internet connectivity in the connected wifi
	 * client using ipv4 interface</li>
	 * <li>PRE-CONDITION 5 : Verify the internet connectivity in the connected wifi
	 * client using ipv6 interface</li>
	 * <li>Step 1:Perform nslookup for which.opendns.com </li>
	 * <li>Step 2:Perform nslookup for which.opendns.com </li>
	 * <li>Step 3:Perform nslookup for which.opendns.com </li>
	 * <li>Step 4:Perform nslookup for which.opendns.com </li>
	 * <li>Step 5:Perform nslookup for which.opendns.com </li>
	 * <li>POST-CONDITION 1 : Verify disconnecting the 2.4GHZ private wifi ssid</li>
	 * </ol>
	 * 
	 * @param device {@link Dut}
	 * @author Muthukumar
	 * @refactor Alan_Bivera
	 */
	@Test(dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true)
	@TestDetails(testUID = "TC-RDKB-2GHZ-DNS-NSLOOKUP-5001")
	public void testToVerifyAlternateDnsNSlookupFor2GHz(Dut device) {

		Dut deviceConnectedWith2Ghz = null;
		String testCaseId = "TC-RDKB-2GHZ-DNS-NSLOOKUP-501";
		int stepNumber = 1;
		stepNum = "S" + stepNumber;
		errorMessage = null;
		status = false;
		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-2GHZ-DNS-NSLOOKUP-5001");
		LOGGER.info("TEST DESCRIPTION: Verifying the alternate DNS nslookup functionality for 2.4GHz");
		LOGGER.info("TEST STEPS : ");
		LOGGER.info("PRE-CONDITION 1 : Connect the client setup to 2.4 GHz SSID and verify connection status");
		LOGGER.info("PRE-CONDITION 2 : Verify  the correct IPv4  address for client connected with 2.4 GHz SSID");
		LOGGER.info("PRE-CONDITION 3 : Verify  the correct IPv6  address for client connected with 2.4 GHz SSID");
		LOGGER.info(
				"PRE-CONDITION 4 : Verify the internet connectivity in the connected wifi client using ipv4 interface");
		LOGGER.info(
				"PRE-CONDITION 5 : Verify the internet connectivity in the connected wifi client using ipv6 interface");
		LOGGER.info("1. Perform nslookup for which.opendns.com");
		LOGGER.info("2. Perform nslookup for which.opendns.com");
		LOGGER.info("3. Perform nslookup for which.opendns.com");
		LOGGER.info("4. Perform nslookup for which.opendns.com with");
		LOGGER.info("5. Perform nslookup for which.opendns.com with");
		LOGGER.info("POST-CONDITION 1 : Verify disconnecting the 2.4GHZ private wifi ssid");
		LOGGER.info("#######################################################################################");
		try {
			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS");
			/**
			 * PRECONDITION 1-5 : connecting client setup to 2.4 and verifying internet
			 * connectivity using IPV4 and IPV6 interface
			 */
			deviceConnectedWith2Ghz = BroadBandPreConditionUtils.executePreConditionToVerifyWiFiClientStatus(device,
					tapEnv, BroadBandTestConstants.BAND_2_4GHZ);
			LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");
			verifyDnsNsLookupFunctionality(device, deviceConnectedWith2Ghz, testCaseId);
		} catch (Exception e) {
			errorMessage = e.getMessage();
			LOGGER.error("EXCEPTION OCCURRED WHILE VERIFYING THE ALTERNATE DNS NSLOOKUP FUNCTIONALITY FOR 2.4GHz "
					+ errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					true);
		} finally {
			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			LOGGER.info("POST-CONDITION STEPS");
			/**
			 * Post-condition 1 : Disconnect the 2.4 GHz private wifi SSID
			 */
			BroadBandPostConditionUtils.executePostConditionToDisconnectClientsConnectedWith2Ghz(device, tapEnv,
					deviceConnectedWith2Ghz, BroadBandTestConstants.CONSTANT_1);
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-2GHZ-DNS-NSLOOKUP-5001");

	}

	/**
	 * Test Case : Verifying the alternate DNS nslookup functionality for 5 GHz
	 * <p>
	 * STEPS:
	 * </p>
	 * <ol>
	 * <li>Test Description</li>
	 * <li>PRE-CONDITION 1 : Connect the client setup to 2.4 GHz SSID and verify
	 * connection status</li>
	 * <li>PRE-CONDITION 2 : Verify the correct IPv4 address for client connected
	 * with 5 GHz SSID</li>
	 * <li>PRE-CONDITION 3 : Verify the correct IPv6 address for client connected
	 * with 5 GHz SSID</li>
	 * <li>PRE-CONDITION 4 : Verify the internet connectivity in the connected wifi
	 * client using ipv4 interface</li>
	 * <li>PRE-CONDITION 5 : Verify the internet connectivity in the connected wifi
	 * client using ipv6 interface</li>
	 * <li>Step 1:Perform nslookup for which.opendns.com</li>
	 * <li>Step 2:Perform nslookup for which.opendns.com</li>
	 * <li>Step 3:Perform nslookup for which.opendns.com</li>
	 * <li>Step 4:Perform nslookup for which.opendns.com</li>
	 * <li>Step 5:Perform nslookup for which.opendns.com</li>
	 * <li>POST-CONDITION 1 : Verify disconnecting the 5GHZ private wifi ssid</li>
	 * </ol>
	 * 
	 * @param device {@link Dut}
	 * @author Muthukumar
	 * @refactor Alan_Bivera
	 */
	@Test(dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true)
	@TestDetails(testUID = "TC-RDKB-5GHZ-DNS-NSLOOKUP-5001")
	public void testToVerifyAlternateDnsNSlookupFor5GHz(Dut device) {

		Dut deviceConnectedWith5Ghz = null;
		String testCaseId = "TC-RDKB-5GHZ-DNS-NSLOOKUP-501";
		int stepNumber = 1;
		stepNum = "S" + stepNumber;
		errorMessage = null;
		status = false;
		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-5GHZ-DNS-NSLOOKUP-5001");
		LOGGER.info("TEST DESCRIPTION: Verifying the alternate DNS nslookup functionality for 5GHz");
		LOGGER.info("TEST STEPS : ");
		LOGGER.info("PRE-CONDITION 1 : Connect the client setup to 5 GHz SSID and verify connection status");
		LOGGER.info("PRE-CONDITION 2 : Verify  the correct IPv4  address for client connected with 5 GHz SSID ");
		LOGGER.info("PRE-CONDITION 3 : Verify  the correct IPv6  address for client connected with 5 GHz SSID ");
		LOGGER.info(
				"PRE-CONDITION 4 : Verify the internet connectivity in the connected wifi client using ipv4 interface");
		LOGGER.info(
				"PRE-CONDITION 5 : Verify the internet connectivity in the connected wifi client using ipv6 interface");
		LOGGER.info("1. Perform nslookup for which.opendns.com with IPV4 interface");
		LOGGER.info("2. Perform nslookup for which.opendns.com with IPV6 interface");
		LOGGER.info("3. Perform nslookup for which.opendns.com with IPV6 interface");
		LOGGER.info("4. Perform nslookup for which.opendns.com with IPV6 interface ");
		LOGGER.info("5. Perform nslookup for which.opendns.com with IPV6 interface ");
		LOGGER.info("POST-CONDITION 1 : Verify disconnecting the 5GHZ private wifi ssid");
		LOGGER.info("#######################################################################################");
		try {
			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS");
			/**
			 * PRECONDITION 1-5 : connecting client setup to 5 GHz and verifying internet
			 * connectivity using IPV4 and IPV6 interface
			 */
			deviceConnectedWith5Ghz = BroadBandPreConditionUtils.executePreConditionToVerifyWiFiClientStatus(device,
					tapEnv, BroadBandTestConstants.BAND_5GHZ);
			LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");
			verifyDnsNsLookupFunctionality(device, deviceConnectedWith5Ghz, testCaseId);
		} catch (Exception e) {
			errorMessage = e.getMessage();
			LOGGER.error("EXCEPTION OCCURRED WHILE VERIFYING THE ALTERNATE DNS NSLOOKUP FUNCTIONALITY FOR 5GHz "
					+ errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					true);
		} finally {
			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			LOGGER.info("POST-CONDITION STEPS");
			/**
			 * Post-condition 1 : Disconnect the 5 GHz private wifi SSID
			 */
			BroadBandPostConditionUtils.executePostConditionToDisconnectClientsConnectedWith5Ghz(device, tapEnv,
					deviceConnectedWith5Ghz, BroadBandTestConstants.CONSTANT_1);
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-5GHZ-DNS-NSLOOKUP-5001");
	}

	/**
	 * Test step method used to validate nslookup functionality
	 * 
	 * @param device          {@link Dut}
	 * @param deviceConnected instance of connected device
	 * @refactor Alan_Bivera
	 * 
	 */
	public static void verifyDnsNsLookupFunctionality(Dut device, Dut deviceConnected, String testCaseId) {

		stepNumber = 1;
		stepNum = "S" + stepNumber;
		errorMessage = "";
		status = false;
		/**
		 * STEP 1: PERFORM NSLOOKUP FOR WHICH.OPENDNS.COM
		 */
		status = false;
		errorMessage = null;
		LOGGER.info("#######################################################################################");
		LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : PERFORM NSLOOKUP FOR WHICH.OPENDNS.COM");
		LOGGER.info(
				"STEP " + stepNumber + ": ACTION : EXECUTE COMMAND: NSLOOKUP -TYPE=TXT WHICH.OPENDNS.COM");
		LOGGER.info("STEP " + stepNumber + ": EXPECTED : MUST RETURN THE RESPONSE AS 'I AM NOT AN OPENDNS RESOLVER'");
		LOGGER.info("#######################################################################################");
		errorMessage = "FAILED TO VERIFY NSLOOKUP";
		status = BroadBandCommonUtils.executeAndVerifyNsLookUpCommandInConnectedClient(deviceConnected, tapEnv,
				BroadBandTestConstants.STRING_DEFAULT_GLOBAL_DNS_IPV4_VALUE,
				BroadBandTestConstants.NS_LOOKUP_GLOBAL_DNS_RESPONSE);
		if (status) {
			LOGGER.info("STEP " + stepNumber + ": ACTUAL : OBTAINED EXPECTED RESULT SUCCESSFULLY.EXPECTED RESULT : "
					+ BroadBandTestConstants.NS_LOOKUP_GLOBAL_DNS_RESPONSE);
		} else {
			LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
		}
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
		LOGGER.info("#######################################################################################");

		/**
		 * STEP 2: PERFORM NSLOOKUP FOR WHICH.OPENDNS.COM WITH 208.67.222.222
		 */
		stepNumber++;
		stepNum = "S" + stepNumber;
		status = false;
		errorMessage = null;
		LOGGER.info("#######################################################################################");
		LOGGER.info(
				"STEP " + stepNumber + ": DESCRIPTION : PERFORM NSLOOKUP FOR WHICH.OPENDNS.COM WITH 208.67.222.222");
		LOGGER.info("STEP " + stepNumber
				+ ": ACTION : EXECUTE COMMAND: NSLOOKUP -TYPE=TXT WHICH.OPENDNS.COM. 208.67.222.222");
		LOGGER.info("STEP " + stepNumber + ": EXPECTED : MUST RETURN THE RESPONSE AS 'RESOLVER1.OPENDNS.COM'");
		LOGGER.info("#######################################################################################");
		errorMessage = "FAILED TO VERIFY NSLOOKUP FOR 208.67.222.222";
		status = BroadBandCommonUtils.executeAndVerifyNsLookUpCommandInConnectedClient(deviceConnected, tapEnv,
				BroadBandTestConstants.NS_LOOKUP_IP_208_67_222_222, BroadBandTestConstants.SERVER_NAME_208_67_222_222);
		if (status) {
			LOGGER.info("STEP " + stepNumber + ": ACTUAL : OBTAINED EXPECTED RESULT SUCCESSFULLY.EXPECTED RESULT : "
					+ BroadBandTestConstants.SERVER_NAME_208_67_222_222);
		} else {
			LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
		}
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
		LOGGER.info("#######################################################################################");

		/**
		 * STEP 3: Perform nslookup for which.opendns.com with 208.67.220.220
		 */
		stepNumber++;
		stepNum = "S" + stepNumber;
		status = false;
		errorMessage = null;
		LOGGER.info("#######################################################################################");
		LOGGER.info(
				"STEP " + stepNumber + ": DESCRIPTION : PERFORM NSLOOKUP FOR WHICH.OPENDNS.COM WITH 208.67.220.220");
		LOGGER.info("STEP " + stepNumber
				+ ": ACTION : EXECUTE COMMAND: NSLOOKUP -TYPE=TXT WHICH.OPENDNS.COM. 208.67.220.220");
		LOGGER.info("STEP " + stepNumber + ": EXPECTED : MUST RETURN THE RESPONSE AS 'RESOLVER2.OPENDNS.COM'");
		LOGGER.info("#######################################################################################");
		errorMessage = "FAILED TO VERIFY NSLOOKUP FOR 208.67.220.220";
		status = BroadBandCommonUtils.executeAndVerifyNsLookUpCommandInConnectedClient(deviceConnected, tapEnv,
				BroadBandTestConstants.NS_LOOKUP_IP_208_67_220_220, BroadBandTestConstants.SERVER_NAME_208_67_220_220);
		if (status) {
			LOGGER.info("STEP " + stepNumber + ": ACTUAL : OBTAINED EXPECTED RESULT SUCCESSFULLY.EXPECTED RESULT : "
					+ BroadBandTestConstants.SERVER_NAME_208_67_220_220);
		} else {
			LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
		}
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
		LOGGER.info("#######################################################################################");

		/**
		 * STEP 4: PERFORM NSLOOKUP FOR WHICH.OPENDNS.COM WITH 208.67.222.220
		 */
		stepNumber++;
		stepNum = "S" + stepNumber;
		status = false;
		errorMessage = null;
		LOGGER.info("#######################################################################################");
		LOGGER.info(
				"STEP " + stepNumber + ": DESCRIPTION : PERFORM NSLOOKUP FOR WHICH.OPENDNS.COM WITH 208.67.222.220 ");
		LOGGER.info("STEP " + stepNumber
				+ ": ACTION : EXECUTE COMMAND: NSLOOKUP -TYPE=TXT WHICH.OPENDNS.COM. 208.67.222.220 ");
		LOGGER.info("STEP " + stepNumber + ": EXPECTED : MUST RETURN THE RESPONSE AS 'RESOLVER3.OPENDNS.COM'");
		LOGGER.info("#######################################################################################");
		errorMessage = "FAILED TO VERIFY NSLOOKUP FOR 208.67.222.220";
		status = BroadBandCommonUtils.executeAndVerifyNsLookUpCommandInConnectedClient(deviceConnected, tapEnv,
				BroadBandTestConstants.NS_LOOKUP_IP_208_67_222_220, BroadBandTestConstants.SERVER_NAME_208_67_222_220);
		if (status) {
			LOGGER.info("STEP " + stepNumber + ": ACTUAL : OBTAINED EXPECTED RESULT SUCCESSFULLY.EXPECTED RESULT : "
					+ BroadBandTestConstants.SERVER_NAME_208_67_222_220);
		} else {
			LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
		}
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
		LOGGER.info("#######################################################################################");

		/**
		 * STEP 5: PERFORM NSLOOKUP FOR WHICH.OPENDNS.COM WITH 208.67.220.222
		 */
		stepNumber++;
		stepNum = "S" + stepNumber;
		status = false;
		errorMessage = null;
		LOGGER.info("#######################################################################################");
		LOGGER.info(
				"STEP " + stepNumber + ": DESCRIPTION : PERFORM NSLOOKUP FOR WHICH.OPENDNS.COM WITH 208.67.220.222 ");
		LOGGER.info("STEP " + stepNumber
				+ ": ACTION : EXECUTE COMMAND: NSLOOKUP -TYPE=TXT WHICH.OPENDNS.COM. 208.67.220.222 ");
		LOGGER.info("STEP " + stepNumber + ": EXPECTED : MUST RETURN THE RESPONSE AS 'RESOLVER4.OPENDNS.COM'");
		LOGGER.info("#######################################################################################");
		errorMessage = "Failed to verify nslookup for 208.67.220.222";
		status = BroadBandCommonUtils.executeAndVerifyNsLookUpCommandInConnectedClient(deviceConnected, tapEnv,
				BroadBandTestConstants.NS_LOOKUP_IP_208_67_220_222, BroadBandTestConstants.SERVER_NAME_208_67_220_222);
		if (status) {
			LOGGER.info("STEP " + stepNumber + ": ACTUAL : OBTAINED EXPECTED RESULT SUCCESSFULLY.EXPECTED RESULT : "
					+ BroadBandTestConstants.SERVER_NAME_208_67_220_222);
		} else {
			LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
		}
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
		LOGGER.info("#######################################################################################");
	}

}
