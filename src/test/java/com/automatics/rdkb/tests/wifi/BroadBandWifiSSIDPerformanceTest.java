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
import com.automatics.rdkb.TestGroup;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.rdkb.utils.wifi.connectedclients.BroadBandConnectedClientUtils;
import com.automatics.rdkb.utils.BroadBandPreConditionUtils;

public class BroadBandWifiSSIDPerformanceTest extends AutomaticsTestBase{
	
    /**
     * Integer to store Iteration value 20 for Wifi Performance test
     */
    public static final Integer WIFI_CNTY_PERFORMANCE_MAXIMUM_NUMBER_OF_ITERATION = 20;
    
    
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
