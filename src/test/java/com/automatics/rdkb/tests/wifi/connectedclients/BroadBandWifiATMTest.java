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
import com.automatics.test.AutomaticsTestBase;
import com.automatics.device.Dut;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.dmcli.DmcliUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;

/**
 * Test class for the verification of Air Time Management group
 * 
 */

public class BroadBandWifiATMTest extends AutomaticsTestBase {
	/**
	 *
	 * <p>
	 * STEPS:
	 * </p>
	 * <ol>
	 * <li>S1) Do factory reset using webpa set operation</li>
	 * <li>S2) Add 2 Stations in APGroup1</li>
	 * <li>S3) Assigning ATM percentage for the Stations (100% for Sta1 and 0% for
	 * Sta2)</li>
	 * <li>S4) Verify the assigned values by entering the dmcli Commands</li>
	 * </ol>
	 * 
	 * @param device
	 * 
	 * @author DEEPIKA S
	 * @Refactor Athira
	 */
	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = BroadBandTestGroup.WIFI)
	@TestDetails(testUID = "TC-RDKB-WIFI-ATM-1002")
	public void testVerificationOfStationsPercentage(Dut device) {
		// Variable Declaration begins
		String stepNum = "s1";
		String testCaseId = "TC-RDKB-WIFI-ATM-102";
		String errorMessage = null;
		boolean status = false;
		String webPaParam = null;
		String valueToBePassed = null;
		// Variable Declaration Ends
		try {
			LOGGER.info("#######################################################################################");
			LOGGER.info("STARTING TEST CASE: TC-RBKB-WIFI-ATM-1002");
			LOGGER.info(
					"TEST DESCRIPTION: Validate AirTimeManagement by Assigning Percentages to the Stations added via dmcli. (2.4Ghz)");
			LOGGER.info("STEP 1: Do factory reset using webpa set operation");
			LOGGER.info("STEP 2: Add 2 Stations  in APGroup1");
			LOGGER.info("STEP 3: Assigning ATM percentage for the Stations (100% for Sta1 and 0% for Sta2)");
			LOGGER.info("STEP 4: Verify the assigned values by entering the dmcli Commands");
			LOGGER.info("#######################################################################################");
			/**
			 * Step 1 : Factory reset the device to check default values
			 */
			stepNum = "s1";
			status = false;
			errorMessage = "Factory Resetting the device failed";
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 1: DESCRIPTION : Factory reset the device to check default values");
			LOGGER.info("STEP 1: ACTION : performing factory reset by webpa");
			LOGGER.info("STEP 1: EXPECTED : The device should get factory resetted by webpa");
			LOGGER.info("**********************************************************************************");
			status = BroadBandCommonUtils.performFactoryResetWebPa(tapEnv, device);
			if (status) {
				LOGGER.info("STEP 1: ACTUAL : Factory Reset is successful");
			} else {
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
			/**
			 * Step 2 : Add 2 Stations in APGroup1
			 */
			stepNum = "s2";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 2: DESCRIPTION : Add 2 sations in APGroup Level ");
			LOGGER.info("STEP 2: ACTION :Add 2 stations in APGroup level by executing webpa command:"
					+ BroadBandWebPaConstants.WEBPA_ATM_GROUP);
			LOGGER.info("STEP 2: EXPECTED : Must return the status of the command executed");
			LOGGER.info("**********************************************************************************");
			for (int iteration = BroadBandTestConstants.CONSTANT_1; iteration <= BroadBandTestConstants.CONSTANT_2; iteration++) {
				errorMessage = BroadBandCommonUtils.concatStringUsingStringBuffer(" Error in adding station ",
						Integer.toString(iteration), " to APGroup1 ");
				webPaParam = BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandWebPaConstants.WEBPA_ATM_GROUP,
						BroadBandTestConstants.STRING_VALUE_ONE, BroadBandWebPaConstants.WEBPA_ATM_GROUP_ADD_STATIONS);
				status = DmcliUtils.addTableUsingDmcliCommand(device, tapEnv, webPaParam);
				if (!status) {
					break;
				}
			}
			if (status) {
				LOGGER.info("STEP 2: ACTUAL : Successfully added 2 stations at APGroup level 1");
			} else {
				LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			/**
			 * Step 3 : Assigning ATM percentage for the Stations (100% for Sta1 and 0% for
			 * Sta2)
			 */
			stepNum = "s3";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 3: DESCRIPTION : Assigning ATM percentage for the Stations (100% for Sta1 and 0% for Sta2)");
			LOGGER.info(
					"STEP 3: ACTION : Assign ATM percentage to the added stations.Execute command:dmcli eRT setv Device.WiFi.X_RDKCENTRAL-COM_ATM.APGroup.1.Sta.1.AirTimePercent");
			LOGGER.info("STEP 3: EXPECTED : Must return the status of the executed command");
			LOGGER.info("**********************************************************************************");

			for (int iteration = BroadBandTestConstants.CONSTANT_1; iteration <= BroadBandTestConstants.CONSTANT_2; iteration++) {
				errorMessage = BroadBandCommonUtils.concatStringUsingStringBuffer(
						"Assigning percentage to ATM station ", Integer.toString(iteration), " failed");
				if (iteration % BroadBandTestConstants.CONSTANT_2 == BroadBandTestConstants.CONSTANT_0) {
					valueToBePassed = BroadBandTestConstants.STRING_ZERO;
				} else {
					valueToBePassed = BroadBandTestConstants.STRING_VALUE_HUNDRED;
				}
				webPaParam = BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandWebPaConstants.WEBPA_ATM_GROUP,
						BroadBandTestConstants.STRING_VALUE_ONE, BroadBandWebPaConstants.WEBPA_ATM_GROUP_ADD_STATIONS,
						Integer.toString(iteration), BroadBandWebPaConstants.WEBPA_ATM_GROUP_PERCENTAGE);
				status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv, webPaParam,
						BroadBandTestConstants.CONSTANT_2, valueToBePassed);
				if (!status) {
					break;
				}
			}
			if (status) {
				LOGGER.info("STEP 3: ACTUAL : Successfully assigned percentage to 2 stations of APgroup level1");
			} else {
				LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
			/**
			 * Step 4 : Verify the assigned values by entering the Following Commands
			 */
			stepNum = "s4";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 4: DESCRIPTION : Verify the assigned values by entering the Following Commandss");
			LOGGER.info(
					"STEP 4: ACTION : Check the APgroup level values.Execute command: dmcli eRT getv Device.WiFi.X_RDKCENTRAL-COM_ATM.APGroup.1.Sta.1.AirTimePercent  ");
			LOGGER.info("STEP 4: EXPECTED : Must return the value of the executed command");
			LOGGER.info("**********************************************************************************");
			for (int iteration = BroadBandTestConstants.CONSTANT_1; iteration <= BroadBandTestConstants.CONSTANT_2; iteration++) {
				errorMessage = BroadBandCommonUtils.concatStringUsingStringBuffer(
						"Assigned ATM APgroup level- station percentage", Integer.toString(iteration), " mismatch");
				if (iteration % BroadBandTestConstants.CONSTANT_2 == BroadBandTestConstants.CONSTANT_0) {
					valueToBePassed = BroadBandTestConstants.STRING_ZERO;
				} else {
					valueToBePassed = BroadBandTestConstants.STRING_VALUE_HUNDRED;
				}
				webPaParam = BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandWebPaConstants.WEBPA_ATM_GROUP,
						BroadBandTestConstants.STRING_VALUE_ONE, BroadBandWebPaConstants.WEBPA_ATM_GROUP_ADD_STATIONS,
						Integer.toString(iteration), BroadBandWebPaConstants.WEBPA_ATM_GROUP_PERCENTAGE);
				status = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcliAndVerify(device, tapEnv, webPaParam,
						valueToBePassed);
				if (!status) {
					break;
				}
			}
			if (status) {
				LOGGER.info("STEP 4: ACTUAL : Successfully verified APgroup level1 stations percentage");
			} else {
				LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			LOGGER.error("EXCEPTION OCCURRED ASSIGNING PERCENTAGE TO STATIONS OF APGROUP LEVEL " + errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					true);
		}
	}
}
